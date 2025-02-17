// Copyright 2018 The Alephium Authors
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

package org.oxygenium.flow.io

import org.oxygenium.flow.setting.AlephiumConfigFixture
import org.oxygenium.io.RocksDBSource
import org.oxygenium.io.RocksDBSource.ColumnFamily
import org.oxygenium.protocol.Generators
import org.oxygenium.protocol.model.BrokerInfo
import org.oxygenium.util.{AlephiumSpec, AVector}

class BrokerStorageSpec
    extends AlephiumSpec
    with StorageSpec[BrokerRocksDBStorage]
    with AlephiumConfigFixture {

  override val dbname: String = "broker-storage-spec"
  override val builder: RocksDBSource => BrokerRocksDBStorage =
    source => BrokerRocksDBStorage(source, ColumnFamily.Broker)

  it should "add/get/delete for BrokerState" in {
    val brokerInfos = AVector.fill(10)(Generators.brokerInfoGen.sample.get)
    brokerInfos.foreach(storage.addBroker(_) isE ())
    storage.activeBrokers().rightValue.toSet is brokerInfos.toSet
    brokerInfos.foreach(info => storage.remove(info.peerId) isE ())
    storage.activeBrokers().rightValue is AVector.empty[BrokerInfo]
  }
}
