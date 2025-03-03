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

import org.oxygenium.protocol.model.Address
import org.oxygenium.util.{AVector, TimeStamp}

@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
final case class Destination(
    address: Address.Asset,
    attoOxmAmount: Option[Amount] = None,
    tokens: Option[AVector[Token]] = None,
    lockTime: Option[TimeStamp] = None,
    message: Option[ByteString] = None
) {
  def getAttoOxmAmount(): Amount = {
    attoOxmAmount.getOrElse(Amount.Zero)
  }
}
