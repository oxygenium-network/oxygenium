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

import org.oxygenium.api.{badRequest, Try}
import org.oxygenium.protocol.model.{Address, BlockHash}
import org.oxygenium.protocol.vm
import org.oxygenium.protocol.vm.{GasBox, GasPrice, StatefulContract}
import org.oxygenium.serde._
import org.oxygenium.util.AVector

@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
final case class BuildDeployContractTx(
    fromPublicKey: ByteString,
    fromPublicKeyType: Option[BuildTxCommon.PublicKeyType] = None,
    bytecode: ByteString,
    initialAttoOxmAmount: Option[Amount] = None,
    initialTokenAmounts: Option[AVector[Token]] = None,
    issueTokenAmount: Option[Amount] = None,
    issueTokenTo: Option[Address.Asset] = None,
    gasAmount: Option[GasBox] = None,
    gasPrice: Option[GasPrice] = None,
    targetBlockHash: Option[BlockHash] = None
) extends BuildTxCommon
    with BuildTxCommon.FromPublicKey {
  def decodeBytecode(): Try[BuildDeployContractTx.Code] = {
    deserialize[BuildDeployContractTx.Code](bytecode).left.map(serdeError =>
      badRequest(serdeError.getMessage)
    )
  }
}

object BuildDeployContractTx {
  final case class Code(
      contract: StatefulContract,
      initialImmFields: AVector[vm.Val],
      initialMutFields: AVector[vm.Val]
  )
  object Code {
    implicit val serde: Serde[Code] = {
      val _serde: Serde[Code] =
        Serde.forProduct3(Code.apply, t => (t.contract, t.initialImmFields, t.initialMutFields))

      _serde.validate(code =>
        if (code.contract.validate(code.initialImmFields, code.initialMutFields)) {
          Right(())
        } else {
          Left(
            s"Invalid field length, expect ${code.contract.fieldLength}, " +
              s"have ${code.initialImmFields.length} immutable fields and " +
              s"${code.initialMutFields.length} mutable fields"
          )
        }
      )
    }
  }
}
