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

package org.oxygenium.flow.setting

import org.oxygenium.protocol.model.NetworkId
import org.oxygenium.util.{Env, SocketUtil}

trait RandomPortsConfigFixture extends SocketUtil {
  private val publicPort   = generatePort()
  private val masterPort   = generatePort()
  private val restPort     = generatePort()
  private val wsPort       = generatePort()
  private val minerApiPort = generatePort()

  lazy val configPortsValues: Map[String, Any] = {
    val networkId = Env.currentEnv match {
      case Env.Test        => NetworkId.OxygeniumDevNet.id
      case Env.Integration => 4 // A testnet that is different from public testnet (id = 1)
      case _               => throw new RuntimeException("Invalid test env")
    }
    Map(
      ("oxygenium.network.network-id", networkId),
      ("oxygenium.network.bind-address", s"127.0.0.1:$publicPort"),
      ("oxygenium.network.external-address", s"127.0.0.1:$publicPort"),
      ("oxygenium.network.internal-address", s"127.0.0.1:$publicPort"),
      ("oxygenium.network.coordinator-address", s"127.0.0.1:$masterPort"),
      ("oxygenium.network.rest-port", restPort),
      ("oxygenium.network.ws-port", wsPort),
      ("oxygenium.network.miner-api-port", minerApiPort)
    )
  }
}
