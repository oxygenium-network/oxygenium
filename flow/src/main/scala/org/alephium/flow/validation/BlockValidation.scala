// Copyright 2018 The Alephium Authors
// This file is part of the alephium project.
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

package org.alephium.flow.validation

import org.alephium.flow.core.{BlockFlow, BlockFlowGroupView}
import org.alephium.flow.model.BlockFlowTemplate
import org.alephium.io.{IOError, IOUtils}
import org.alephium.protocol.{ALPH, Hash}
import org.alephium.protocol.config.{BrokerConfig, ConsensusConfigs, NetworkConfig}
import org.alephium.protocol.mining.Emission
import org.alephium.protocol.model._
import org.alephium.protocol.vm.{BlockEnv, GasPrice, LockupScript, LogConfig, WorldState}
import org.alephium.serde._
import org.alephium.util.{AVector, Bytes, EitherF, U256}

// scalastyle:off number.of.methods

trait BlockValidation extends Validation[Block, InvalidBlockStatus, Option[WorldState.Cached]] {
  import ValidationStatus._

  implicit def networkConfig: NetworkConfig

  def headerValidation: HeaderValidation
  def nonCoinbaseValidation: TxValidation

  override def validate(
      block: Block,
      flow: BlockFlow
  ): BlockValidationResult[Option[WorldState.Cached]] = {
    checkBlock(block, flow)
  }

  def validateTemplate(
      chainIndex: ChainIndex,
      template: BlockFlowTemplate,
      blockFlow: BlockFlow
  ): BlockValidationResult[Option[WorldState.Cached]] = {
    val dummyHeader = BlockHeader.unsafe(
      BlockDeps.unsafe(template.deps),
      template.depStateHash,
      Hash.zero,
      template.templateTs,
      template.target,
      Nonce.zero
    )
    val dummyBlock = Block(dummyHeader, template.transactions)
    checkTemplate(chainIndex, dummyBlock, blockFlow)
  }

  // keep the commented lines so we could compare it easily with `checkBlockAfterHeader`
  def checkTemplate(
      chainIndex: ChainIndex,
      block: Block,
      flow: BlockFlow
  ): BlockValidationResult[Option[WorldState.Cached]] = {
    for {
//      _ <- checkGroup(block)
//      _ <- checkNonEmptyTransactions(block)
      _ <- checkTxNumber(block)
      _ <- checkGasPriceDecreasing(block)
      _ <- checkTotalGas(block, networkConfig.getHardFork(block.timestamp))
//      _ <- checkMerkleRoot(block)
//      _ <- checkFlow(block, flow)
      sideResult <- checkTxs(chainIndex, block, flow)
    } yield sideResult
  }

  override def validateUntilDependencies(
      block: Block,
      flow: BlockFlow
  ): BlockValidationResult[Unit] = {
    checkBlockUntilDependencies(block, flow)
  }

  def validateAfterDependencies(
      block: Block,
      flow: BlockFlow
  ): BlockValidationResult[Option[WorldState.Cached]] = {
    checkBlockAfterDependencies(block, flow)
  }

  private[validation] def checkBlockUntilDependencies(
      block: Block,
      flow: BlockFlow
  ): BlockValidationResult[Unit] = {
    headerValidation.checkHeaderUntilDependencies(block.header, flow)
  }

  private[validation] def checkBlockAfterDependencies(
      block: Block,
      flow: BlockFlow
  ): BlockValidationResult[Option[WorldState.Cached]] = {
    for {
      _          <- headerValidation.checkHeaderAfterDependencies(block.header, flow)
      sideResult <- checkBlockAfterHeader(block, flow)
    } yield sideResult
  }

  private[validation] def checkBlock(
      block: Block,
      flow: BlockFlow
  ): BlockValidationResult[Option[WorldState.Cached]] = {
    for {
      _          <- headerValidation.checkHeader(block.header, flow)
      sideResult <- checkBlockAfterHeader(block, flow)
    } yield sideResult
  }

  private[flow] def checkBlockAfterHeader(
      block: Block,
      flow: BlockFlow
  ): BlockValidationResult[Option[WorldState.Cached]] = {
    for {
      _          <- checkGroup(block)
      _          <- checkNonEmptyTransactions(block)
      _          <- checkTxNumber(block)
      _          <- checkGasPriceDecreasing(block)
      _          <- checkTotalGas(block, networkConfig.getHardFork(block.timestamp))
      _          <- checkMerkleRoot(block)
      _          <- checkFlow(block, flow)
      sideResult <- checkTxs(block.chainIndex, block, flow)
    } yield sideResult
  }

  private[validation] def checkGhostUncles(
      flow: BlockFlow,
      chainIndex: ChainIndex,
      block: Block,
      ghostUncleHashes: AVector[BlockHash]
  ): BlockValidationResult[AVector[(LockupScript.Asset, Int)]] = {
    if (brokerConfig.contains(chainIndex.from)) {
      val hardFork = networkConfig.getHardFork(block.timestamp)
      if (hardFork.isGhostEnabled() && ghostUncleHashes.nonEmpty) {
        val blockchain = flow.getBlockChain(chainIndex)
        for {
          _ <- checkGhostUncleSize(ghostUncleHashes)
          _ <- checkGhostUncleOrder(ghostUncleHashes)
          uncleBlocks <- ghostUncleHashes.mapE(blockchain.getBlock) match {
            case Left(IOError.KeyNotFound(_)) => invalidBlock(GhostUncleDoesNotExist)
            case result                       => from(result)
          }
          _            <- validateGhostUncles(flow, chainIndex, block, uncleBlocks)
          _            <- checkGhostUncleDeps(block, flow, uncleBlocks)
          parentHeight <- from(blockchain.getHeight(block.uncleHash(chainIndex.to)))
          uncleHeights <- from(ghostUncleHashes.mapE(blockchain.getHeight))
        } yield uncleBlocks.zipWithIndex.map { case (uncleBlock, index) =>
          val uncleHeight = uncleHeights(index)
          (uncleBlock.minerLockupScript, parentHeight + 1 - uncleHeight)
        }
      } else if (ghostUncleHashes.nonEmpty) {
        invalidBlock(InvalidGhostUnclesBeforeGhostHardFork)
      } else {
        validBlock(AVector.empty)
      }
    } else {
      validBlock(AVector.empty)
    }
  }

  private def validateGhostUncles(
      flow: BlockFlow,
      chainIndex: ChainIndex,
      block: Block,
      uncles: AVector[Block]
  ): BlockValidationResult[Unit] = {
    assume(uncles.nonEmpty)
    val blockchain = flow.getBlockChain(chainIndex)
    for {
      parentHeader           <- from(blockchain.getBlockHeader(block.uncleHash(chainIndex.to)))
      usedUnclesAndAncestors <- from(blockchain.getUsedGhostUnclesAndAncestors(parentHeader))
      (usedUncles, ancestors) = usedUnclesAndAncestors
      _ <- uncles.foreachE { uncle =>
        if (uncle.hash == parentHeader.hash || ancestors.exists(_ == uncle.hash)) {
          invalidBlock(NotGhostUnclesForTheBlock)
        } else if (!ancestors.exists(_ == uncle.parentHash)) {
          invalidBlock(GhostUncleHashConflictWithParentHash)
        } else if (usedUncles.contains(uncle.hash)) {
          invalidBlock(GhostUnclesAlreadyUsed)
        } else {
          validBlock(())
        }
      }
    } yield ()
  }

  @inline private def checkGhostUncleSize(
      uncles: AVector[BlockHash]
  ): BlockValidationResult[Unit] = {
    if (uncles.length > ALPH.MaxUncleSize) {
      invalidBlock(InvalidGhostUncleSize)
    } else {
      validBlock(())
    }
  }

  @inline private[validation] def checkGhostUncleOrder(
      uncles: AVector[BlockHash]
  ): BlockValidationResult[Unit] = {
    uncles.foreachWithIndexE { case (hash, index) =>
      if (index > 0) {
        if (Bytes.byteStringOrdering.compare(hash.bytes, uncles(index - 1).bytes) <= 0) {
          invalidBlock(UnsortedGhostUncles)
        } else {
          validBlock(())
        }
      } else {
        validBlock(())
      }
    }
  }

  @inline private def checkGhostUncleDeps(
      block: Block,
      flow: BlockFlow,
      uncles: AVector[Block]
  ): BlockValidationResult[Unit] = {
    for {
      isValid <- from(
        IOUtils.tryExecute(
          uncles.forall(uncle => flow.isExtendingUnsafe(block.blockDeps, uncle.blockDeps))
        )
      )
      _ <-
        if (isValid) {
          validBlock(())
        } else {
          invalidBlock(InvalidGhostUncleDeps)
        }
    } yield ()
  }

  private def checkTxs(
      chainIndex: ChainIndex,
      block: Block,
      flow: BlockFlow
  ): BlockValidationResult[Option[WorldState.Cached]] = {
    if (brokerConfig.contains(chainIndex.from)) {
      val hardFork = networkConfig.getHardFork(block.timestamp)
      for {
        groupView <- from(flow.getMutableGroupView(chainIndex.from, block.blockDeps))
        _         <- checkNonCoinbases(chainIndex, block, groupView, hardFork)
        _ <- checkCoinbase(
          flow,
          chainIndex,
          block,
          groupView,
          hardFork
        ) // validate non-coinbase first for gas fee
      } yield {
        if (chainIndex.isIntraGroup) Some(groupView.worldState) else None
      }
    } else {
      validBlock(None)
    }
  }

  private[validation] def checkGroup(block: Block): BlockValidationResult[Unit] = {
    if (block.chainIndex.relateTo(brokerConfig)) {
      validBlock(())
    } else {
      invalidBlock(InvalidGroup)
    }
  }

  private[validation] def checkNonEmptyTransactions(block: Block): BlockValidationResult[Unit] = {
    if (block.transactions.nonEmpty) validBlock(()) else invalidBlock(EmptyTransactionList)
  }

  private[validation] def checkTxNumber(block: Block): BlockValidationResult[Unit] = {
    if (block.transactions.length <= maximalTxsInOneBlock) {
      validBlock(())
    } else {
      invalidBlock(TooManyTransactions)
    }
  }

  private[validation] def checkGasPriceDecreasing(block: Block): BlockValidationResult[Unit] = {
    val result = block.transactions.foldE[Unit, GasPrice](GasPrice(ALPH.MaxALPHValue)) {
      case (lastGasPrice, tx) =>
        val txGasPrice = tx.unsigned.gasPrice
        if (txGasPrice > lastGasPrice) Left(()) else Right(txGasPrice)
    }
    if (result.isRight) validBlock(()) else invalidBlock(TxGasPriceNonDecreasing)
  }

  // Let's check the gas is decreasing as well
  private[validation] def checkTotalGas(
      block: Block,
      hardFork: HardFork
  ): BlockValidationResult[Unit] = {
    val totalGas = block.transactions.fold(0)(_ + _.unsigned.gasAmount.value)
    val maximalGas =
      if (hardFork.isGhostEnabled()) maximalGasPerBlock else maximalGasPerBlockPreRhone
    if (totalGas <= maximalGas.value) validBlock(()) else invalidBlock(TooMuchGasUsed)
  }

  private[validation] def checkCoinbase(
      flow: BlockFlow,
      chainIndex: ChainIndex,
      block: Block,
      groupView: BlockFlowGroupView[WorldState.Cached],
      hardFork: HardFork
  ): BlockValidationResult[Unit] = {
    val consensusConfig = consensusConfigs.getConsensusConfig(hardFork)
    val result = consensusConfig.emission.reward(block.header) match {
      case Emission.PoW(miningReward) =>
        val netReward = Transaction.totalReward(block.gasFee, miningReward, hardFork)
        checkCoinbase(flow, chainIndex, block, groupView, 1, netReward, netReward, hardFork)
      case Emission.PoLW(miningReward, burntAmount) =>
        val lockedReward = Transaction.totalReward(block.gasFee, miningReward, hardFork)
        val netReward    = lockedReward.subUnsafe(burntAmount)
        checkCoinbase(flow, chainIndex, block, groupView, 2, netReward, lockedReward, hardFork)
    }

    result match {
      case Left(Right(ExistInvalidTx(_, InvalidAlphBalance))) => Left(Right(InvalidCoinbaseReward))
      case result                                             => result
    }
  }

  private[validation] def checkRewardPreGhost(
      chainIndex: ChainIndex,
      block: Block,
      groupView: BlockFlowGroupView[WorldState.Cached],
      netReward: U256,
      lockedReward: U256
  ): BlockValidationResult[Unit] = {
    for {
      _ <- checkCoinbaseAsTx(chainIndex, block, groupView, netReward.addUnsafe(coinbaseGasFee))
      _ <- checkLockedReward(block, AVector(lockedReward))
    } yield ()
  }

  private[validation] def checkRewardGhost(
      chainIndex: ChainIndex,
      block: Block,
      groupView: BlockFlowGroupView[WorldState.Cached],
      netReward: U256,
      lockedReward: U256,
      uncles: AVector[(LockupScript.Asset, Int)]
  ): BlockValidationResult[Unit] = {
    val mainChainReward = Coinbase.calcMainChainReward(netReward)
    val uncleRewards = uncles.map(uncle => Coinbase.calcGhostUncleReward(mainChainReward, uncle._2))
    val blockReward  = Coinbase.calcBlockReward(mainChainReward, uncleRewards)
    val blockRewardLocked = blockReward.mulUnsafe(lockedReward).divUnsafe(netReward)

    val coinbase           = block.coinbase.unsigned
    val isBlockRewardValid = coinbase.fixedOutputs.head.amount == blockReward
    val uncleRewardOutputs = coinbase.fixedOutputs.tail
    val isUncleRewardValid = uncleRewards.length == uncleRewardOutputs.length &&
      uncleRewards.zipWithIndex.forall { case (reward, index) =>
        val output = uncleRewardOutputs(index)
        uncles(index)._1 == output.lockupScript && reward == output.amount
      }
    if (isBlockRewardValid && isUncleRewardValid) {
      val totalReward = uncleRewards.fold(blockReward)(_ addUnsafe _)
      for {
        _ <- checkCoinbaseAsTx(chainIndex, block, groupView, totalReward.addUnsafe(coinbaseGasFee))
        _ <- checkLockedReward(block, blockRewardLocked +: uncleRewards)
      } yield ()
    } else {
      invalidBlock(InvalidCoinbaseReward)
    }
  }

  private[validation] def checkCoinbase(
      flow: BlockFlow,
      chainIndex: ChainIndex,
      block: Block,
      groupView: BlockFlowGroupView[WorldState.Cached],
      outputNum: Int,
      netReward: U256,
      lockedReward: U256,
      hardFork: HardFork
  ): BlockValidationResult[Unit] = {
    for {
      ghostUncleHashes <- checkCoinbaseData(chainIndex, block)
      totalOutputNum = outputNum + ghostUncleHashes.length
      _      <- checkCoinbaseEasy(block, totalOutputNum)
      uncles <- checkGhostUncles(flow, chainIndex, block, ghostUncleHashes)
      _ <-
        if (hardFork.isGhostEnabled()) {
          checkRewardGhost(chainIndex, block, groupView, netReward, lockedReward, uncles)
        } else {
          checkRewardPreGhost(chainIndex, block, groupView, netReward, lockedReward)
        }
    } yield ()
  }

  private[validation] def checkCoinbaseAsTx(
      chainIndex: ChainIndex,
      block: Block,
      groupView: BlockFlowGroupView[WorldState.Cached],
      netReward: U256
  ): BlockValidationResult[Unit] = {
    if (brokerConfig.contains(chainIndex.from)) {
      val blockEnv = BlockEnv.from(chainIndex, block.header)
      convert(
        block.coinbase,
        nonCoinbaseValidation.checkBlockTx(
          chainIndex,
          block.coinbase,
          groupView,
          blockEnv,
          Some(netReward)
        )
      )
    } else {
      validBlock(())
    }
  }

  private[validation] def checkCoinbaseEasy(
      block: Block,
      outputsNum: Int
  ): BlockValidationResult[Unit] = {
    val coinbase = block.coinbase // Note: validateNonEmptyTransactions first pls!
    val unsigned = coinbase.unsigned
    if (
      unsigned.scriptOpt.isEmpty &&
      unsigned.gasAmount == minimalGas &&
      unsigned.gasPrice == coinbaseGasPrice &&
      unsigned.fixedOutputs.length == outputsNum &&
      unsigned.fixedOutputs(0).tokens.isEmpty &&
      coinbase.contractInputs.isEmpty &&
      coinbase.generatedOutputs.isEmpty &&
      coinbase.inputSignatures.isEmpty &&
      coinbase.scriptSignatures.isEmpty
    ) {
      validBlock(())
    } else {
      invalidBlock(InvalidCoinbaseFormat)
    }
  }

  private[validation] def checkCoinbaseData(
      chainIndex: ChainIndex,
      block: Block
  ): BlockValidationResult[AVector[BlockHash]] = {
    val coinbase = block.coinbase
    if (coinbase.unsigned.fixedOutputs.isEmpty) {
      invalidBlock(InvalidCoinbaseFormat)
    } else {
      val data = coinbase.unsigned.fixedOutputs.head.additionalData
      deserialize[CoinbaseData](data) match {
        case Right(CoinbaseDataV1(prefix, _)) =>
          if (prefix == CoinbaseDataPrefix.from(chainIndex, block.timestamp)) {
            validBlock(AVector.empty)
          } else {
            invalidBlock(InvalidCoinbaseData)
          }
        case Right(CoinbaseDataV2(prefix, ghostUncleHashes, _)) =>
          if (prefix == CoinbaseDataPrefix.from(chainIndex, block.timestamp)) {
            validBlock(ghostUncleHashes)
          } else {
            invalidBlock(InvalidCoinbaseData)
          }
        case Left(_) => invalidBlock(InvalidCoinbaseData)
      }
    }
  }

  private[validation] def checkLockedReward(
      block: Block,
      lockedAmounts: AVector[U256]
  ): BlockValidationResult[Unit] = {
    val outputs = block.coinbase.unsigned.fixedOutputs
    assume(outputs.length >= lockedAmounts.length) // PoLW would have more outputs
    val lockTime = block.timestamp.plusUnsafe(networkConfig.coinbaseLockupPeriod)
    outputs.take(lockedAmounts.length).foreachWithIndexE { case (output, index) =>
      if (output.amount != lockedAmounts(index)) {
        invalidBlock(InvalidCoinbaseLockedAmount)
      } else if (output.lockTime != lockTime) {
        invalidBlock(InvalidCoinbaseLockupPeriod)
      } else {
        validBlock(())
      }
    }
  }

  private[validation] def checkMerkleRoot(block: Block): BlockValidationResult[Unit] = {
    if (block.header.txsHash == Block.calTxsHash(block.transactions)) {
      validBlock(())
    } else {
      invalidBlock(InvalidTxsMerkleRoot)
    }
  }

  private[validation] def checkNonCoinbases(
      chainIndex: ChainIndex,
      block: Block,
      groupView: BlockFlowGroupView[WorldState.Cached],
      hardFork: HardFork
  ): BlockValidationResult[Unit] = {
    assume(chainIndex.relateTo(brokerConfig))

    if (brokerConfig.contains(chainIndex.from)) {
      for {
        _ <- checkBlockDoubleSpending(block)
        _ <- checkNonCoinbasesEach(chainIndex, block, groupView, hardFork)
      } yield ()
    } else {
      validBlock(())
    }
  }

  private[validation] def checkNonCoinbasesEach(
      chainIndex: ChainIndex,
      block: Block,
      groupView: BlockFlowGroupView[WorldState.Cached],
      hardFork: HardFork
  ): BlockValidationResult[Unit] = {
    val blockEnv       = BlockEnv.from(chainIndex, block.header)
    val parentHash     = block.blockDeps.uncleHash(chainIndex.to)
    val executionOrder = Block.getNonCoinbaseExecutionOrder(parentHash, block.nonCoinbase, hardFork)
    EitherF.foreachTry(executionOrder) { index =>
      val tx = block.transactions(index)
      val txValidationResult = nonCoinbaseValidation.checkBlockTx(
        chainIndex,
        tx,
        groupView,
        blockEnv,
        None
      )
      if (ALPH.isSequentialTxSupported(chainIndex, hardFork)) {
        tx.unsigned.fixedOutputRefs.foreachWithIndex { case (outputRef, outputIndex) =>
          blockEnv.addOutputRef(outputRef, tx.unsigned.fixedOutputs(outputIndex))
        }
      }
      txValidationResult match {
        case Right(_) => Right(())
        case Left(Right(TxScriptExeFailed(_))) =>
          if (tx.contractInputs.isEmpty) {
            Right(())
          } else {
            convert(tx, invalidTx(ContractInputsShouldBeEmptyForFailedTxScripts))
          }
        case Left(Right(e)) => convert(tx, invalidTx(e))
        case Left(Left(e))  => Left(Left(e))
      }
    }
  }

  private[validation] def checkBlockDoubleSpending(block: Block): BlockValidationResult[Unit] = {
    val utxoUsed = scala.collection.mutable.Set.empty[TxOutputRef]
    block.nonCoinbase.foreachE { tx =>
      tx.unsigned.inputs.foreachE { input =>
        if (utxoUsed.contains(input.outputRef)) {
          invalidBlock(BlockDoubleSpending)
        } else {
          utxoUsed += input.outputRef
          validBlock(())
        }
      }
    }
  }

  private[validation] def checkFlow(block: Block, blockFlow: BlockFlow)(implicit
      brokerConfig: BrokerConfig
  ): BlockValidationResult[Unit] = {
    if (brokerConfig.contains(block.chainIndex.from)) {
      ValidationStatus.from(blockFlow.checkFlowTxs(block)).flatMap { ok =>
        if (ok) validBlock(()) else invalidBlock(InvalidFlowTxs)
      }
    } else {
      validBlock(())
    }
  }
}

object BlockValidation {
  def build(blockFlow: BlockFlow): BlockValidation =
    build(
      blockFlow.brokerConfig,
      blockFlow.networkConfig,
      blockFlow.consensusConfigs,
      blockFlow.logConfig
    )

  def build(implicit
      brokerConfig: BrokerConfig,
      networkConfig: NetworkConfig,
      consensusConfigs: ConsensusConfigs,
      logConfig: LogConfig
  ): BlockValidation = new Impl()

  class Impl(implicit
      val brokerConfig: BrokerConfig,
      val networkConfig: NetworkConfig,
      val consensusConfigs: ConsensusConfigs,
      val logConfig: LogConfig
  ) extends BlockValidation {
    override def headerValidation: HeaderValidation  = HeaderValidation.build
    override def nonCoinbaseValidation: TxValidation = TxValidation.build
  }
}
