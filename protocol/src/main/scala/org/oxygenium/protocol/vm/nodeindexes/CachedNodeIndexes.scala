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

import org.oxygenium.io.{CachedKVStorage, IOResult}
import org.oxygenium.protocol.model.TxOutputRef
import org.oxygenium.protocol.vm.event.CachedLog
import org.oxygenium.protocol.vm.nodeindexes.TxIdTxOutputLocators
import org.oxygenium.protocol.vm.subcontractindex.CachedSubContractIndex

final case class CachedNodeIndexes(
    logStorageCache: CachedLog,
    txOutputRefIndexCache: Option[CachedKVStorage[TxOutputRef.Key, TxIdTxOutputLocators]],
    subContractIndexCache: Option[CachedSubContractIndex]
) {
  def persist(): IOResult[NodeIndexesStorage] = {
    for {
      logStorage <- logStorageCache.persist()
      txOutputRefIndexStorage <- txOutputRefIndexCache match {
        case Some(cache) => cache.persist().map(Some(_))
        case None        => Right(None)
      }
      subContractIndexStorage <- subContractIndexCache match {
        case Some(cache) => cache.persist().map(Some(_))
        case None        => Right(None)
      }
    } yield NodeIndexesStorage(
      logStorage,
      txOutputRefIndexStorage,
      subContractIndexStorage
    )
  }

  def staging(): StagingNodeIndexes = {
    new StagingNodeIndexes(
      logStorageCache.staging(),
      txOutputRefIndexCache.map(_.staging()),
      subContractIndexCache.map(_.staging())
    )
  }
}

object CachedNodeIndexes {
  @inline def from(nodeIndexesStorage: NodeIndexesStorage): CachedNodeIndexes = {
    new CachedNodeIndexes(
      CachedLog.from(nodeIndexesStorage.logStorage),
      nodeIndexesStorage.txOutputRefIndexStorage.map(CachedKVStorage.from),
      nodeIndexesStorage.subContractIndexStorage.map(CachedSubContractIndex.from)
    )
  }
}
