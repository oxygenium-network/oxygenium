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

import org.oxygenium.io.StagingKVStorage
import org.oxygenium.protocol.model.TxOutputRef
import org.oxygenium.protocol.vm.event.StagingLog
import org.oxygenium.protocol.vm.nodeindexes.TxIdTxOutputLocators
import org.oxygenium.protocol.vm.subcontractindex.StagingSubContractIndex

final case class StagingNodeIndexes(
    logState: StagingLog,
    txOutputRefIndexState: Option[StagingKVStorage[TxOutputRef.Key, TxIdTxOutputLocators]],
    subContractIndexState: Option[StagingSubContractIndex]
) {
  def rollback(): Unit = {
    logState.rollback()
    txOutputRefIndexState.foreach(_.rollback())
    subContractIndexState.foreach(_.rollback())
    ()
  }

  def commit(): Unit = {
    logState.commit()
    txOutputRefIndexState.foreach(_.commit())
    subContractIndexState.foreach(_.commit())
    ()
  }
}
