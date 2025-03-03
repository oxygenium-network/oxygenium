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

import org.oxygenium.protocol.model
import org.oxygenium.protocol.model.{ContractId, ContractOutput}
import org.oxygenium.protocol.vm.LockupScript
import org.oxygenium.util.{AVector, U256}

@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
final case class AssetState(attoOxmAmount: U256, tokens: Option[AVector[Token]] = None) {
  lazy val flatTokens: AVector[Token] = tokens.getOrElse(AVector.empty)

  def toContractOutput(contractId: ContractId): ContractOutput = {
    ContractOutput(
      attoOxmAmount,
      LockupScript.p2c(contractId),
      flatTokens.map(token => (token.id, token.amount))
    )
  }
}

object AssetState {
  def from(attoOxmAmount: U256, tokens: AVector[Token]): AssetState = {
    AssetState(attoOxmAmount, Some(tokens))
  }

  def from(output: model.TxOutput): AssetState = {
    AssetState.from(output.amount, output.tokens.map(pair => Token(pair._1, pair._2)))
  }
}
