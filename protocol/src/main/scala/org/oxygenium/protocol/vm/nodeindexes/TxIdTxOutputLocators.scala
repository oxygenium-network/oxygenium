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
package org.oxygenium.protocol.vm.nodeindexes

import org.oxygenium.protocol.model.TransactionId
import org.oxygenium.serde.{avectorSerde, Serde}
import org.oxygenium.util.AVector

final case class TxIdTxOutputLocators(
    txId: TransactionId,
    txOutputLocators: AVector[TxOutputLocator]
)

object TxIdTxOutputLocators {
  implicit val txIdTxOutputLocatorsSerde: Serde[TxIdTxOutputLocators] =
    Serde.forProduct2(apply, b => (b.txId, b.txOutputLocators))
}
