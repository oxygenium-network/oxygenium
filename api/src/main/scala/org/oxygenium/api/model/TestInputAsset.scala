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

package org.oxygenium.api.model

import akka.util.ByteString

import org.oxygenium.protocol.model.{Address, AssetOutput}
import org.oxygenium.protocol.vm
import org.oxygenium.protocol.vm._
import org.oxygenium.util.{AVector, TimeStamp, U256}

final case class TestInputAsset(address: Address.Asset, asset: AssetState) {
  def toAssetOutput: AssetOutput =
    AssetOutput(
      asset.attoOxmAmount,
      address.lockupScript,
      TimeStamp.zero,
      asset.flatTokens.map(token => (token.id, token.amount)),
      ByteString.empty
    )

  def approveAll(gasFeeOpt: Option[U256]): AVector[Instr[StatefulContext]] = {
    val addressConst = AddressConst(vm.Val.Address(address.lockupScript))
    val attoOxmAmount = gasFeeOpt match {
      case Some(gasFee) => asset.attoOxmAmount.subUnsafe(gasFee)
      case None         => asset.attoOxmAmount
    }
    val alphInstrs = AVector[Instr[StatefulContext]](
      addressConst,
      U256Const(vm.Val.U256(attoOxmAmount)),
      ApproveOxm
    )
    val tokenInstrs = asset.flatTokens.flatMap[Instr[StatefulContext]] { token =>
      AVector(
        addressConst,
        BytesConst(vm.Val.ByteVec(token.id.bytes)),
        U256Const(vm.Val.U256(token.amount)),
        ApproveToken
      )
    }
    alphInstrs ++ tokenInstrs
  }
}
