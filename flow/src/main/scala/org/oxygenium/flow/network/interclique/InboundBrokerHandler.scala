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

package org.oxygenium.flow.network.interclique

import java.net.InetSocketAddress

import akka.actor.Props
import akka.io.Tcp

import org.oxygenium.flow.core.BlockFlow
import org.oxygenium.flow.handler.AllHandlers
import org.oxygenium.flow.network.CliqueManager
import org.oxygenium.flow.network.broker.{InboundBrokerHandler => BaseInboundBrokerHandler}
import org.oxygenium.flow.network.sync.BlockFlowSynchronizer
import org.oxygenium.flow.setting.NetworkSetting
import org.oxygenium.protocol.config.BrokerConfig
import org.oxygenium.protocol.model.CliqueInfo
import org.oxygenium.util.ActorRefT

object InboundBrokerHandler {
  // scalastyle:off parameter.number
  def props(
      selfCliqueInfo: CliqueInfo,
      remoteAddress: InetSocketAddress,
      connection: ActorRefT[Tcp.Command],
      blockflow: BlockFlow,
      allHandlers: AllHandlers,
      cliqueManager: ActorRefT[CliqueManager.Command],
      blockFlowSynchronizer: ActorRefT[BlockFlowSynchronizer.Command]
  )(implicit brokerConfig: BrokerConfig, networkSetting: NetworkSetting): Props =
    Props(
      new InboundBrokerHandler(
        selfCliqueInfo,
        remoteAddress,
        connection,
        blockflow,
        allHandlers,
        cliqueManager,
        blockFlowSynchronizer
      )
    )
  // scalastyle:on
}

class InboundBrokerHandler(
    val selfCliqueInfo: CliqueInfo,
    val remoteAddress: InetSocketAddress,
    val connection: ActorRefT[Tcp.Command],
    val blockflow: BlockFlow,
    val allHandlers: AllHandlers,
    val cliqueManager: ActorRefT[CliqueManager.Command],
    val blockFlowSynchronizer: ActorRefT[BlockFlowSynchronizer.Command]
)(implicit val brokerConfig: BrokerConfig, val networkSetting: NetworkSetting)
    extends BrokerHandler
    with BaseInboundBrokerHandler
