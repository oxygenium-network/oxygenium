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

package org.oxygenium.protocol.vm

import scala.collection.mutable

import org.scalatest.Assertion

import org.oxygenium.protocol.OXM
import org.oxygenium.protocol.config.{GroupConfig, NetworkConfigFixture}
import org.oxygenium.protocol.model._
import org.oxygenium.util.{OxygeniumSpec, AVector, U256}
import org.oxygenium.util.Bytes.byteStringOrdering

class MutBalancesPerLockupSpec extends OxygeniumSpec {

  it should "tokenVector" in new Fixture {
    val tokens = mutable.Map(tokenId -> OXM.oneOxm)
    MutBalancesPerLockup(OXM.oneOxm, tokens, 1).tokenVector is AVector((tokenId, OXM.oneOxm))

    val tokenIdZero = TokenId.generate
    tokens.addOne((tokenIdZero, U256.Zero))

    MutBalancesPerLockup(OXM.oneOxm, tokens, 1).tokenVector is AVector((tokenId, OXM.oneOxm))

    tokens.remove(tokenIdZero)

    forAll(hashGen) { newTokenIdValue =>
      tokens.addOne((TokenId.unsafe(newTokenIdValue), OXM.oneOxm))
      MutBalancesPerLockup(OXM.oneOxm, tokens, 1).tokenVector is AVector.from(
        tokens.toSeq.sortBy(_._1.bytes)
      )
    }
  }

  it should "getTokenAmount" in new Fixture {
    val tokenId2 = TokenId.generate
    val tokens   = mutable.Map(tokenId -> U256.One, tokenId2 -> U256.Two)

    val balancesPerLockup = MutBalancesPerLockup(OXM.oneOxm, tokens, 1)

    balancesPerLockup.getTokenAmount(tokenId) is Some(U256.One)
    balancesPerLockup.getTokenAmount(tokenId2) is Some(U256.Two)
    balancesPerLockup.getTokenAmount(TokenId.generate) is None
  }

  it should "addOxm" in new Fixture {
    val balancesPerLockup = MutBalancesPerLockup(OXM.oneOxm, mutable.Map.empty, 1)

    var current = OXM.oneOxm

    forAll(amountGen(1)) { amount =>
      current.add(amount) match {
        case Some(newCurrent) =>
          current = newCurrent
          balancesPerLockup.addOxm(amount) is Some(())
        case None =>
          balancesPerLockup.addOxm(amount) is None
      }
      balancesPerLockup.attoOxmAmount is current
    }

    balancesPerLockup.addOxm(U256.MaxValue) is None
  }

  it should "addToken" in new Fixture {
    val tokens            = mutable.Map(tokenId -> OXM.oneOxm)
    val balancesPerLockup = MutBalancesPerLockup(OXM.oneOxm, tokens, 1)

    balancesPerLockup.addToken(tokenId, OXM.oneOxm) is Some(())
    balancesPerLockup.getTokenAmount(tokenId) is Some(OXM.alph(2))

    balancesPerLockup.addToken(tokenId, U256.MaxValue) is None
    balancesPerLockup.getTokenAmount(tokenId) is Some(OXM.alph(2))

    val tokenId2 = TokenId.generate
    balancesPerLockup.getTokenAmount(tokenId2) is None
    balancesPerLockup.addToken(tokenId2, OXM.oneOxm) is Some(())
    balancesPerLockup.getTokenAmount(tokenId2) is Some(OXM.oneOxm)
  }

  it should "subOxm" in new Fixture {
    val balancesPerLockup = MutBalancesPerLockup(U256.HalfMaxValue, mutable.Map.empty, 1)

    var current = U256.HalfMaxValue

    forAll(amountGen(1)) { amount =>
      current.sub(amount) match {
        case Some(newCurrent) =>
          current = newCurrent
          balancesPerLockup.subOxm(amount) is Some(())
        case None =>
          balancesPerLockup.subOxm(amount) is None
      }
      balancesPerLockup.attoOxmAmount is current
    }

    balancesPerLockup.subOxm(U256.MaxValue) is None
  }

  it should "subToken" in new Fixture {
    val tokens            = mutable.Map(tokenId -> OXM.oneOxm)
    val balancesPerLockup = MutBalancesPerLockup(OXM.oneOxm, tokens, 1)

    balancesPerLockup.subToken(tokenId, OXM.oneOxm) is Some(())
    balancesPerLockup.getTokenAmount(tokenId) is Some(U256.Zero)

    balancesPerLockup.subToken(tokenId, U256.MaxValue) is None
    balancesPerLockup.getTokenAmount(tokenId) is Some(U256.Zero)

    val tokenId2 = TokenId.generate
    balancesPerLockup.getTokenAmount(tokenId2) is None
    balancesPerLockup.subToken(tokenId2, OXM.oneOxm) is None
    balancesPerLockup.getTokenAmount(tokenId2) is None
  }

  it should "add" in new Fixture {
    val balancesPerLockup =
      MutBalancesPerLockup(OXM.oneOxm, mutable.Map(tokenId -> OXM.oneOxm), 1)

    val tokenId2 = TokenId.generate
    val balancesPerLockup2 = MutBalancesPerLockup(
      OXM.oneOxm,
      mutable.Map(tokenId -> OXM.oneOxm, tokenId2 -> OXM.oneOxm),
      1
    )

    balancesPerLockup.add(balancesPerLockup2) is Some(())
    balancesPerLockup is MutBalancesPerLockup(
      OXM.alph(2),
      mutable.Map(tokenId -> OXM.alph(2), tokenId2 -> OXM.oneOxm),
      1
    )

    balancesPerLockup.add(MutBalancesPerLockup(U256.MaxValue, mutable.Map.empty, 1)) is None
    balancesPerLockup.add(
      MutBalancesPerLockup(OXM.oneOxm, mutable.Map(tokenId -> U256.MaxValue), 1)
    ) is None
  }

  it should "sub" in new Fixture {
    val balancesPerLockup =
      MutBalancesPerLockup(OXM.oneOxm, mutable.Map(tokenId -> OXM.oneOxm), 1)

    val tokenId2 = TokenId.generate
    val balancesPerLockup2 =
      MutBalancesPerLockup(OXM.oneOxm, mutable.Map(tokenId -> OXM.oneOxm), 1)

    balancesPerLockup.sub(balancesPerLockup2) is Some(())
    balancesPerLockup is MutBalancesPerLockup(U256.Zero, mutable.Map(tokenId -> U256.Zero), 1)

    balancesPerLockup.sub(MutBalancesPerLockup(U256.MaxValue, mutable.Map.empty, 1)) is None
    balancesPerLockup.sub(
      MutBalancesPerLockup(OXM.oneOxm, mutable.Map(tokenId -> U256.MaxValue), 1)
    ) is None
    balancesPerLockup.sub(
      MutBalancesPerLockup(OXM.oneOxm, mutable.Map(tokenId2 -> OXM.oneOxm), 1)
    ) is None
  }

  trait ToTxOutputFixture extends Fixture {
    import org.oxygenium.protocol.model.TokenId.tokenIdOrder

    val lockupScript = lockupScriptGen.sample.get
    val tokens       = AVector.fill(maxTokenPerContractUtxo + 1)(TokenId.generate).sorted
    val tokenId0     = tokens(0)
    val tokenId1     = tokens(1)

    case class Test(alphAmount: U256, tokens: (TokenId, U256)*) {
      lazy val genesisOutputs = MutBalancesPerLockup(alphAmount, mutable.Map.from(tokens), 1)
        .toTxOutput(lockupScript, HardFork.Mainnet)

      lazy val lemanOutputs = MutBalancesPerLockup(alphAmount, mutable.Map.from(tokens), 1)
        .toTxOutput(lockupScript, HardFork.Leman)

      lazy val rhoneOutputs = MutBalancesPerLockup(alphAmount, mutable.Map.from(tokens), 1)
        .toTxOutput(lockupScript, HardFork.Rhone)

      def expectGenesis(outputs: (U256, Seq[(TokenId, U256)])*): Assertion = {
        genesisOutputs isE AVector.from(outputs).map { case (amount, tokens) =>
          TxOutput.fromDeprecated(amount, AVector.from(tokens), lockupScript)
        }
      }

      def failGenesis(): Assertion = {
        genesisOutputs is failed(
          InvalidOutputBalances(lockupScript, tokens.length, alphAmount)
        )
      }

      def expectLeman(outputs: (U256, Seq[(TokenId, U256)])*): Assertion = {
        lemanOutputs isE AVector.from(outputs).map { case (amount, tokens) =>
          TxOutput.fromDeprecated(amount, AVector.from(tokens), lockupScript)
        }
      }

      def failLeman(
          error: ExeFailure = InvalidOutputBalances(lockupScript, tokens.length, alphAmount)
      ): Assertion = {
        lemanOutputs is failed(error)
      }

      def expectRhone(outputs: (U256, Seq[(TokenId, U256)])*): Assertion = {
        rhoneOutputs isE AVector.from(outputs).map { case (amount, tokens) =>
          TxOutput.fromDeprecated(amount, AVector.from(tokens), lockupScript)
        }
      }

      def failRhone(
          error: ExeFailure = InvalidOutputBalances(lockupScript, tokens.length, alphAmount)
      ): Assertion = {
        rhoneOutputs is failed(error)
      }
    }
  }

  it should "toTxOutput for Genesis fork" in new ToTxOutputFixture {
    Test(0).expectGenesis()
    Test(OXM.oneOxm, tokenId -> 1).expectGenesis(
      OXM.oneOxm -> Seq(tokenId -> 1)
    )
    Test(OXM.oneOxm, tokenId0 -> 1, tokenId1 -> 2).expectGenesis(
      OXM.oneOxm -> Seq(tokenId0 -> 1, tokenId1 -> 2)
    )
    Test(OXM.oneOxm).expectGenesis(OXM.oneOxm -> Seq.empty)
    Test(0, tokenId -> 1).failGenesis()
  }

  it should "toTxOutput for Leman fork + asset lockup script" in new ToTxOutputFixture {
    override val lockupScript: LockupScript = assetLockupGen(GroupIndex.unsafe(0)).sample.get

    Test(0).expectLeman()
    Test(dustUtxoAmount - 1).failLeman()
    Test(dustUtxoAmount).expectLeman(dustUtxoAmount -> Seq.empty)
    Test(OXM.oneOxm).expectLeman(OXM.oneOxm     -> Seq.empty)

    Test(0, tokenId -> 1).failLeman()
    Test(dustUtxoAmount - 1, tokenId -> 1).failLeman()
    Test(dustUtxoAmount, tokenId -> 1).expectLeman(
      dustUtxoAmount -> Seq(tokenId -> 1)
    )
    Test(dustUtxoAmount * 2 - 1, tokenId -> 1).failLeman()
    Test(dustUtxoAmount * 2, tokenId -> 1).expectLeman(
      dustUtxoAmount -> Seq(tokenId -> 1),
      dustUtxoAmount -> Seq.empty
    )
    Test(OXM.oneOxm, tokenId -> 1).expectLeman(
      dustUtxoAmount                         -> Seq(tokenId -> 1),
      OXM.oneOxm.subUnsafe(dustUtxoAmount) -> Seq.empty
    )

    Test(0, tokenId0 -> 1, tokenId1 -> 2).failLeman()
    Test(dustUtxoAmount * 2 - 1, tokenId0 -> 1, tokenId1 -> 2).failLeman()
    Test(dustUtxoAmount * 2, tokenId0 -> 1, tokenId1 -> 2).expectLeman(
      dustUtxoAmount -> Seq(tokenId0 -> 1),
      dustUtxoAmount -> Seq(tokenId1 -> 2)
    )
    Test(dustUtxoAmount * 3 - 1, tokenId0 -> 1, tokenId1 -> 2).failLeman()
    Test(dustUtxoAmount * 3, tokenId0 -> 1, tokenId1 -> 2).expectLeman(
      dustUtxoAmount -> Seq(tokenId0 -> 1),
      dustUtxoAmount -> Seq(tokenId1 -> 2),
      dustUtxoAmount -> Seq.empty
    )
    Test(OXM.oneOxm, tokenId0 -> 1, tokenId1 -> 2).expectLeman(
      dustUtxoAmount                             -> Seq(tokenId0 -> 1),
      dustUtxoAmount                             -> Seq(tokenId1 -> 2),
      OXM.oneOxm.subUnsafe(dustUtxoAmount * 2) -> Seq.empty
    )
  }

  it should "toTxOutput for Leman fork + contract lockup script" in new ToTxOutputFixture {
    override val lockupScript: LockupScript = LockupScript.p2c(ContractId.generate)
    val address                             = Address.from(lockupScript)

    Test(0).expectLeman()
    Test(OXM.oneOxm - 1).failLeman(LowerThanContractMinimalBalance(address, OXM.oneOxm - 1))
    Test(OXM.oneOxm).expectLeman(OXM.oneOxm -> Seq.empty)

    Test(0, tokenId -> 1).failLeman()
    Test(OXM.oneOxm - 1, tokenId -> 1)
      .failLeman(LowerThanContractMinimalBalance(address, OXM.oneOxm - 1))
    Test(OXM.oneOxm, tokenId -> 1).expectLeman(
      OXM.oneOxm -> Seq(tokenId -> 1)
    )
    Test(OXM.oneOxm, tokens.init.map(_ -> U256.One).toSeq: _*).expectLeman(
      OXM.oneOxm -> tokens.init.map(_ -> U256.One).toSeq
    )
    Test(OXM.oneOxm, tokens.map(_ -> U256.One).toSeq: _*)
      .failLeman(InvalidTokenNumForContractOutput(address, tokens.length))
  }

  it should "toTxOutput for rhone fork + contract lockup script" in new ToTxOutputFixture {
    override val lockupScript: LockupScript = LockupScript.p2c(ContractId.generate)
    val address                             = Address.from(lockupScript)
    val minimalDeposit                      = OXM.oneOxm.divUnsafe(U256.unsafe(10))

    Test(0).expectRhone()
    Test(minimalDeposit - 1).failRhone(LowerThanContractMinimalBalance(address, minimalDeposit - 1))
    Test(minimalDeposit).expectRhone(minimalDeposit -> Seq.empty)

    Test(0, tokenId -> 1).failRhone()
    Test(minimalDeposit - 1, tokenId -> 1)
      .failRhone(LowerThanContractMinimalBalance(address, minimalDeposit - 1))
    Test(minimalDeposit, tokenId -> 1).expectRhone(
      minimalDeposit -> Seq(tokenId -> 1)
    )
    Test(minimalDeposit, tokens.init.map(_ -> U256.One).toSeq: _*).expectRhone(
      minimalDeposit -> tokens.init.map(_ -> U256.One).toSeq
    )
    Test(minimalDeposit, tokens.map(_ -> U256.One).toSeq: _*)
      .failRhone(InvalidTokenNumForContractOutput(address, tokens.length))
  }

  trait Fixture extends TxGenerators with NetworkConfigFixture.Default {
    val tokenId = TokenId.generate

    implicit override val groupConfig: GroupConfig =
      new GroupConfig {
        override def groups: Int = 3
      }
  }
}
