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

package org.oxygenium.io

import org.rocksdb.{ReadOptions, WriteOptions}

import org.oxygenium.io.RocksDBSource.ProdSettings

trait RocksDBKeyValueCompanion[S <: RocksDBKeyValueStorage[_, _]] {
  def apply(storage: RocksDBSource, cf: RocksDBSource.ColumnFamily): S =
    apply(storage, cf, ProdSettings.writeOptions, ProdSettings.readOptions)

  def apply(storage: RocksDBSource, cf: RocksDBSource.ColumnFamily, writeOptions: WriteOptions): S =
    apply(storage, cf, writeOptions, ProdSettings.readOptions)

  def apply(
      storage: RocksDBSource,
      cf: RocksDBSource.ColumnFamily,
      writeOptions: WriteOptions,
      readOptions: ReadOptions
  ): S
}
