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

import org.oxygenium.protocol.{model => protocol}
import org.oxygenium.protocol.Signature
import org.oxygenium.protocol.config.NetworkConfig
import org.oxygenium.serde._
import org.oxygenium.util.AVector

final case class Transaction(
    unsigned: UnsignedTx,
    scriptExecutionOk: Boolean,
    contractInputs: AVector[OutputRef],
    generatedOutputs: AVector[Output],
    inputSignatures: AVector[ByteString],
    scriptSignatures: AVector[ByteString]
) {
  def toProtocol()(implicit networkConfig: NetworkConfig): Either[String, protocol.Transaction] = {
    for {
      unsignedTx <- unsigned.toProtocol()
      inputSig   <- inputSignatures.mapE(deserialize[Signature]).left.map(_.getMessage())
      scriptSig  <- scriptSignatures.mapE(deserialize[Signature]).left.map(_.getMessage())
    } yield {
      protocol.Transaction(
        unsignedTx,
        scriptExecutionOk,
        contractInputs.map(_.unsafeToContractOutputRef()),
        generatedOutputs.map(_.toProtocol()),
        inputSig,
        scriptSig
      )
    }
  }
}

object Transaction {
  def fromProtocol(transaction: protocol.Transaction): Transaction = {
    Transaction(
      UnsignedTx.fromProtocol(transaction.unsigned),
      transaction.scriptExecutionOk,
      transaction.contractInputs.map(OutputRef.from),
      transaction.generatedOutputs.zipWithIndex.map { case (out, index) =>
        Output.from(out, transaction.unsigned.id, index + transaction.unsigned.fixedOutputs.length)
      },
      transaction.inputSignatures.map(sig => serialize(sig)),
      transaction.scriptSignatures.map(sig => serialize(sig))
    )
  }
}
