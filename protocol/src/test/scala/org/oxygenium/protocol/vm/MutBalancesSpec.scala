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
import scala.collection.mutable.ArrayBuffer

import org.oxygenium.protocol.OXM
import org.oxygenium.protocol.config.{GroupConfig, NetworkConfigFixture}
import org.oxygenium.protocol.model.{GroupIndex, TokenId, TxGenerators, TxOutput}
import org.oxygenium.util.{OxygeniumSpec, AVector, U256}

class MutBalancesSpec extends OxygeniumSpec {

  it should "getBalances" in new Fixture {
    balances.getBalances(lockupScript) is Some(balancesPerLockup)
    balances.getBalances(lockupScriptGen.sample.get) is None
  }

  it should "getAttoOxmAmount" in new Fixture {
    balances.getAttoOxmAmount(lockupScript) is Some(balancesPerLockup.attoOxmAmount)
    balances.getAttoOxmAmount(lockupScriptGen.sample.get) is None
  }

  it should "getTokenAmount" in new Fixture {
    balances.getTokenAmount(lockupScript, tokenId) is balancesPerLockup.tokenAmounts.get(tokenId)
    balances.getTokenAmount(lockupScriptGen.sample.get, tokenId) is None
  }

  it should "addAmount" in new Fixture {
    balances.addOxm(lockupScript, OXM.oneOxm) is Some(())

    balances is MutBalances(
      ArrayBuffer((lockupScript, balancesPerLockup.copy(attoOxmAmount = OXM.alph(2))))
    )

    val lockupScript2 = lockupScriptGen.sample.get
    balances.addOxm(lockupScript2, OXM.oneOxm) is Some(())
    balances is MutBalances(
      ArrayBuffer(
        (lockupScript, balancesPerLockup.copy(attoOxmAmount = OXM.alph(2))),
        (lockupScript2, MutBalancesPerLockup(OXM.oneOxm, mutable.Map.empty, 0))
      )
    )

    balances.addOxm(lockupScript, U256.MaxValue) is None
  }

  it should "addToken" in new Fixture {
    balances.addToken(lockupScript, tokenId, OXM.oneOxm) is Some(())

    balances is MutBalances(
      ArrayBuffer(
        (
          lockupScript,
          balancesPerLockup.copy(tokenAmounts = mutable.Map(tokenId -> OXM.alph(2)))
        )
      )
    )

    val tokenId2 = TokenId.generate

    balances.addToken(lockupScript, tokenId2, OXM.oneOxm) is Some(())
    balances is MutBalances(
      ArrayBuffer(
        (
          lockupScript,
          balancesPerLockup.copy(tokenAmounts =
            mutable.Map(tokenId -> OXM.alph(2), tokenId2 -> OXM.alph(1))
          )
        )
      )
    )

    val lockupScript2 = lockupScriptGen.sample.get
    balances.addToken(lockupScript2, tokenId, OXM.oneOxm) is Some(())
    balances is MutBalances(
      ArrayBuffer(
        (
          lockupScript,
          balancesPerLockup.copy(tokenAmounts =
            mutable.Map(tokenId -> OXM.alph(2), tokenId2 -> OXM.alph(1))
          )
        ),
        (lockupScript2, MutBalancesPerLockup(U256.Zero, mutable.Map(tokenId -> OXM.alph(1)), 0))
      )
    )

    balances.addToken(lockupScript, tokenId, U256.MaxValue) is None
  }

  it should "subOxm" in new Fixture {
    balances.subOxm(lockupScript, OXM.oneOxm) is Some(())

    balances is MutBalances(
      ArrayBuffer((lockupScript, balancesPerLockup.copy(attoOxmAmount = U256.Zero)))
    )

    balances.subOxm(lockupScript, U256.MaxValue) is None

    val lockupScript2 = lockupScriptGen.sample.get
    balances.subOxm(lockupScript2, OXM.oneOxm) is None
  }

  it should "subToken" in new Fixture {
    balances.subToken(lockupScript, tokenId, OXM.oneOxm) is Some(())

    balances is MutBalances(
      ArrayBuffer(
        (
          lockupScript,
          balancesPerLockup.copy(tokenAmounts = mutable.Map(tokenId -> U256.Zero))
        )
      )
    )

    val tokenId2 = TokenId.generate

    balances.subToken(lockupScript, tokenId2, OXM.oneOxm) is None

    val lockupScript2 = lockupScriptGen.sample.get
    balances.subToken(lockupScript2, tokenId, OXM.oneOxm) is None

    balances.subToken(lockupScript, tokenId, U256.MaxValue) is None
  }

  it should "add" in new Fixture {

    balances.add(lockupScript, balancesPerLockup) is Some(())

    balances is MutBalances(
      ArrayBuffer(
        (
          lockupScript,
          balancesPerLockup.copy(
            attoOxmAmount = OXM.alph(2),
            tokenAmounts = mutable.Map(tokenId -> OXM.alph(2))
          )
        )
      )
    )

    val lockupScript2 = lockupScriptGen.sample.get
    balances.add(lockupScript2, balancesPerLockup) is Some(())

    balances is MutBalances(
      ArrayBuffer(
        (
          lockupScript,
          balancesPerLockup
            .copy(
              attoOxmAmount = OXM.alph(2),
              tokenAmounts = mutable.Map(tokenId -> OXM.alph(2))
            )
        ),
        (lockupScript2, balancesPerLockup)
      )
    )
  }

  it should "sub" in new Fixture {

    balances.sub(lockupScript, balancesPerLockup) is Some(())

    balances is MutBalances(
      ArrayBuffer(
        (
          lockupScript,
          balancesPerLockup.copy(
            attoOxmAmount = U256.Zero,
            tokenAmounts = mutable.Map(tokenId -> U256.Zero)
          )
        )
      )
    )

    val lockupScript2 = lockupScriptGen.sample.get
    balances.sub(lockupScript2, balancesPerLockup) is None
  }

  it should "use" in new Fixture {

    balances.use() is MutBalances(
      ArrayBuffer((lockupScript, balancesPerLockup.copy(scopeDepth = scopeDepth + 1)))
    )

    balances is MutBalances(ArrayBuffer.empty)
  }

  it should "useAll" in new Fixture {
    val lockupScript2 = lockupScriptGen.sample.get
    balances.add(lockupScript2, balancesPerLockup)

    balances.useAll(lockupScript2) is Some(balancesPerLockup)

    balances is MutBalances(ArrayBuffer((lockupScript, balancesPerLockup)))

    balances.useAll(lockupScript2) is None

    balances.useAll(lockupScript) is Some(balancesPerLockup)

    balances is MutBalances(ArrayBuffer.empty)
  }

  it should "merge" in new Fixture {

    val lockupScript2 = lockupScriptGen.sample.get
    val balances2     = MutBalances(ArrayBuffer((lockupScript2, balancesPerLockup)))

    balances.merge(balances2)

    balances is MutBalances(
      ArrayBuffer((lockupScript, balancesPerLockup), (lockupScript2, balancesPerLockup))
    )

    balances.merge(balances2)

    balances is MutBalances(
      ArrayBuffer(
        (lockupScript, balancesPerLockup),
        (
          lockupScript2,
          balancesPerLockup.copy(
            attoOxmAmount = OXM.alph(2),
            tokenAmounts = mutable.Map(tokenId -> OXM.alph(2))
          )
        )
      )
    )

    val balances3 =
      MutBalances(
        ArrayBuffer((lockupScript2, balancesPerLockup.copy(attoOxmAmount = U256.MaxValue)))
      )

    balances.merge(balances3) is None
  }

  it should "toOutputs" in new Fixture {
    val lockupScript2 = p2pkhLockupGen(GroupIndex.unsafe(0)).sample.get
    val balances2     = MutBalances(ArrayBuffer((lockupScript2, balancesPerLockup)))

    balances.merge(balances2)

    balances.toOutputs() is Some(
      TxOutput.from(OXM.oneOxm, AVector.from(tokens), lockupScript).value ++
        TxOutput.from(OXM.oneOxm, AVector.from(tokens), lockupScript2).value
    )
  }

  trait Fixture extends TxGenerators with NetworkConfigFixture.Default {
    implicit override val groupConfig: GroupConfig =
      new GroupConfig {
        override def groups: Int = 3
      }

    val tokenId    = TokenId.generate
    val scopeDepth = 1
    val tokens     = mutable.Map(tokenId -> OXM.oneOxm)
    val balancesPerLockup =
      MutBalancesPerLockup(OXM.oneOxm, tokens, scopeDepth)
    val lockupScript = p2pkhLockupGen(GroupIndex.unsafe(0)).sample.get

    val balances = MutBalances(ArrayBuffer((lockupScript, balancesPerLockup)))
  }
}
