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

package org.oxygenium.protocol

import java.text.{DecimalFormat, DecimalFormatSymbols}

import org.oxygenium.protocol.model.{Address, ChainIndex, HardFork, Weight}
import org.oxygenium.util.{AVector, Duration, Number, TimeStamp, U256}

object OXM {
  // scalastyle:off magic.number
  val CoinInOneOXM: U256     = U256.unsafe(Number.quintillion)
  val CoinInOneCent: U256     = CoinInOneOXM divUnsafe U256.unsafe(100)
  val CoinInOneNanoOxm: U256 = U256.unsafe(Number.billion)

  val MaxOXMValue: U256 = U256.Billion mulUnsafe CoinInOneOXM

  val GenesisHeight: Int          = 0
  val GenesisWeight: Weight       = Weight.zero
  val GenesisTimestamp: TimeStamp = TimeStamp.unsafe(1231006505000L) // BTC genesis timestamp
  val LaunchTimestamp: TimeStamp  = TimeStamp.unsafe(1636379983000L) // 2021-11-08T11:20:06+00:00

  val OneYear: Duration                                 = Duration.ofDaysUnsafe(365)
  val OneAndHalfYear: Duration                          = Duration.ofDaysUnsafe(365 + 365 / 2)
  val PreLemanDifficultyBombEnabledTimestamp: TimeStamp = LaunchTimestamp.plusUnsafe(OneYear)
  val ExpDiffPeriod: Duration                           = Duration.ofDaysUnsafe(30)
  val DifficultyBombPatchEnabledTimeStamp: TimeStamp =
    TimeStamp.unsafe(1670612400000L) // Dec 09 2022 19:00:00 GMT+0000
  val DifficultyBombPatchHeightDiff: Int = 2700 // around 2 days

  val MaxTxInputNum: Int     = 256
  val MaxTxOutputNum: Int    = 256
  val MaxOutputDataSize: Int = 256
  val MaxScriptSigNum: Int   = 32
  val MaxKeysInP2MPK: Int    = 16

  val MaxGhostUncleAge: Int  = 7
  val MaxGhostUncleSize: Int = 2
  // scalastyle:on magic.number

  def alph(amount: U256): Option[U256] = amount.mul(CoinInOneOXM)

  def alph(amount: Long): U256 = {
    assume(amount >= 0)
    U256.unsafe(amount).mulUnsafe(CoinInOneOXM)
  }

  def cent(amount: Long): U256 = {
    assume(amount >= 0)
    U256.unsafe(amount).mulUnsafe(CoinInOneCent)
  }

  def nanoOxm(amount: Long): U256 = {
    assume(amount >= 0)
    U256.unsafe(amount).mulUnsafe(CoinInOneNanoOxm)
  }

  val oneOxm: U256     = CoinInOneOXM
  val oneNanoOxm: U256 = CoinInOneNanoOxm

  // x.x OXM format
  def alphFromString(string: String): Option[U256] = {
    val regex = """([0-9]*\.?[0-9]+) *OXM""".r
    string match {
      case regex(v) =>
        val bigDecimal = new java.math.BigDecimal(v)
        val scaling    = bigDecimal.scale()
        // scalastyle:off magic.number
        if (scaling > 18) {
          None
        } else {
          U256.from(bigDecimal.movePointRight(18).toBigInteger)
        }
      // scalastyle:on magic.number
      case _ => None
    }
  }

  def prettifyAmount(amount: U256): String = {
    if (amount == U256.Zero) {
      "0 OXM"
    } else {
      val converted = (BigDecimal(amount.v) / BigDecimal(oneOxm.v)).toDouble
      s"${format(converted)} OXM"
    }
  }

  private def format(value: Double): String = {
    // scalastyle:off magic.number
    val decimalFormat = new DecimalFormat()
    decimalFormat.setGroupingUsed(true)
    decimalFormat.setMinimumIntegerDigits(1)
    decimalFormat.setMinimumFractionDigits(1)
    decimalFormat.setMaximumFractionDigits(18)
    // scalastyle:on magic.number

    val symbols = new DecimalFormatSymbols()
    symbols.setDecimalSeparator('.')
    symbols.setGroupingSeparator(',')
    decimalFormat.setDecimalFormatSymbols(symbols)

    decimalFormat.format(value)
  }

  @inline def isSequentialTxSupported(chainIndex: ChainIndex, hardFork: HardFork): Boolean = {
    hardFork.isRhoneEnabled() && chainIndex.isIntraGroup
  }

  lazy val testnetWhitelistedMiners = {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    def miner(address: String) = {
      Address.fromBase58(address).get.lockupScript
    }
    Set(
      miner("1GC2TDVmsprP4khryR4wXoKxinXmbSdmZMXdinFdfkTwi"),
      miner("124oZKw9WkNhBrsW1L2TGjrmhyMoxZi98RotAZXMq9AoF"),
      miner("15TM5iGb9hpN4pWT1u4tXGC59seo5XSc1T29khoPcrwpt"),
      miner("1EFqgJ5aXnkCtK6Cp7jMynEExqHHMCXZ16BJkq78brdn5")
    )
  }

  @inline def isTestnetMinersWhitelisted(miners: AVector[Address.Asset]): Boolean = {
    miners.forall(miner => testnetWhitelistedMiners.contains(miner.lockupScript))
  }
}
