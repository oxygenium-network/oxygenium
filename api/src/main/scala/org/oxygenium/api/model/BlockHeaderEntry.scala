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

import org.oxygenium.protocol.model.{BlockHash, BlockHeader}
import org.oxygenium.util.{AVector, TimeStamp}

final case class BlockHeaderEntry(
    hash: BlockHash,
    timestamp: TimeStamp,
    chainFrom: Int,
    chainTo: Int,
    height: Int,
    deps: AVector[BlockHash]
)
object BlockHeaderEntry {
  def from(header: BlockHeader, height: Int): BlockHeaderEntry = {
    BlockHeaderEntry(
      hash = header.hash,
      timestamp = header.timestamp,
      chainFrom = header.chainIndex.from.value,
      chainTo = header.chainIndex.to.value,
      height = height,
      deps = header.blockDeps.deps
    )
  }
}
