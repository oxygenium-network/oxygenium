// Copyright 2018 The Oxygenium Authors
// This file is part of the oxygenium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see <http://www.gnu.org/licenses/>.

package org.oxygenium.flow.client

import java.nio.file.Path

import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, Future}

import akka.actor.{ActorSystem, Props}
import com.typesafe.scalalogging.StrictLogging

import org.oxygenium.flow.Utils
import org.oxygenium.flow.core._
import org.oxygenium.flow.handler.AllHandlers
import org.oxygenium.flow.io.Storages
import org.oxygenium.flow.network.{Bootstrapper, CliqueManager, DiscoveryServer, TcpController}
import org.oxygenium.flow.network.broker.MisbehaviorManager
import org.oxygenium.flow.network.sync.BlockFlowSynchronizer
import org.oxygenium.flow.setting._
import org.oxygenium.io.RocksDBSource.ProdSettings
import org.oxygenium.protocol.vm.LogConfig
import org.oxygenium.util.{ActorRefT, Env, EventBus, Service}

trait Node extends Service {
  implicit def config: OxygeniumConfig
  def system: ActorSystem
  def blockFlow: BlockFlow
  def misbehaviorManager: ActorRefT[MisbehaviorManager.Command]
  def discoveryServer: ActorRefT[DiscoveryServer.Command]
  def tcpController: ActorRefT[TcpController.Command]
  def bootstrapper: ActorRefT[Bootstrapper.Command]
  def cliqueManager: ActorRefT[CliqueManager.Command]
  def eventBus: ActorRefT[EventBus.Message]
  def allHandlers: AllHandlers

  override def subServices: ArraySeq[Service] = ArraySeq.empty

  override protected def startSelfOnce(): Future[Unit] = Future.successful(())

  override protected def stopSelfOnce(): Future[Unit] = Future.successful(())
}

// scalastyle:off method.length
object Node {
  def build(storages: Storages, flowSystem: ActorSystem)(implicit
      executionContext: ExecutionContext,
      config: OxygeniumConfig
  ): Node = new Default(storages, flowSystem)

  class Default(storages: Storages, flowSystem: ActorSystem)(implicit
      val executionContext: ExecutionContext,
      val config: OxygeniumConfig
  ) extends Node
      with StrictLogging {
    override def serviceName: String = "Node"

    implicit override def system: ActorSystem               = flowSystem
    implicit private val brokerConfig: BrokerSetting        = config.broker
    implicit private val consensusConfig: ConsensusSettings = config.consensus
    implicit private val networkSetting: NetworkSetting     = config.network
    implicit private val discoveryConfig: DiscoverySetting  = config.discovery
    implicit private val miningSetting: MiningSetting       = config.mining
    implicit private val memPoolSetting: MemPoolSetting     = config.mempool
    implicit private val logConfig: LogConfig               = config.node.eventLogConfig

    val blockFlow: BlockFlow = buildBlockFlowUnsafe(storages)

    val misbehaviorManager: ActorRefT[MisbehaviorManager.Command] =
      ActorRefT.build(
        system,
        MisbehaviorManager.props(
          networkSetting.banDuration,
          networkSetting.penaltyForgiveness,
          networkSetting.penaltyFrequency
        )
      )

    val discoveryProps: Props =
      DiscoveryServer.props(
        networkSetting.bindAddress,
        misbehaviorManager,
        storages.brokerStorage,
        config.discovery.bootstrap
      )
    val discoveryServer: ActorRefT[DiscoveryServer.Command] =
      ActorRefT.build[DiscoveryServer.Command](system, discoveryProps)

    val tcpController: ActorRefT[TcpController.Command] =
      ActorRefT
        .build[TcpController.Command](
          system,
          TcpController.props(config.network.bindAddress, misbehaviorManager)
        )

    val eventBus: ActorRefT[EventBus.Message] =
      ActorRefT.build[EventBus.Message](system, EventBus.props())

    val allHandlers: AllHandlers =
      AllHandlers.build(system, blockFlow, eventBus, storages)

    val blockFlowSynchronizer: ActorRefT[BlockFlowSynchronizer.Command] =
      ActorRefT.build(system, BlockFlowSynchronizer.props(blockFlow, allHandlers))
    lazy val cliqueManager: ActorRefT[CliqueManager.Command] =
      ActorRefT.build(
        system,
        CliqueManager.props(
          blockFlow,
          allHandlers,
          discoveryServer,
          blockFlowSynchronizer,
          discoveryConfig.bootstrap.length
        ),
        "CliqueManager"
      )

    val bootstrapper: ActorRefT[Bootstrapper.Command] =
      ActorRefT.build(
        system,
        Bootstrapper.props(tcpController, cliqueManager, storages.nodeStateStorage),
        "Bootstrapper"
      )
  }

  def buildBlockFlowUnsafe(rootPath: Path): (BlockFlow, Storages) = {
    val typesafeConfig =
      Configs.parseConfigAndValidate(Env.Prod, rootPath, overwrite = true)
    val config = OxygeniumConfig.load(typesafeConfig, "oxygenium")
    val dbPath = rootPath.resolve(config.network.networkId.nodeFolder)
    val storages =
      Storages.createUnsafe(dbPath, "db", ProdSettings.writeOptions)(config.broker, config.node)
    buildBlockFlowUnsafe(storages)(config) -> storages
  }

  def buildBlockFlowUnsafe(storages: Storages)(implicit config: OxygeniumConfig): BlockFlow = {
    val nodeStateStorage = storages.nodeStateStorage
    val isInitialized    = Utils.unsafe(nodeStateStorage.isInitialized())
    if (isInitialized) {
      val blockFlow = BlockFlow.fromStorageUnsafe(config, storages)
      checkGenesisBlocks(blockFlow)
      blockFlow
    } else {
      val blockflow = BlockFlow.fromGenesisUnsafe(config, storages)
      Utils.unsafe(nodeStateStorage.setInitialized())
      blockflow
    }
  }

  def checkGenesisBlocks(blockFlow: BlockFlow)(implicit config: OxygeniumConfig): Unit = {
    config.broker.chainIndexes.foreach { chainIndex =>
      val configGenesisBlock = config.genesisBlocks(chainIndex.from.value)(chainIndex.to.value)
      val hashes             = Utils.unsafe(blockFlow.getHashes(chainIndex, 0))
      if (hashes.length != 1 || hashes.head != configGenesisBlock.hash) {
        throw new Exception(invalidGenesisBlockMsg)
      }
    }
  }

  val invalidGenesisBlockMsg =
    "Invalid genesis blocks, please wipe out the db history of your current network and resync"
}
