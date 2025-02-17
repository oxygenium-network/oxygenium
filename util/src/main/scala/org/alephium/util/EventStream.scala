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

package org.oxygenium.util

import akka.actor._

trait EventStream extends EventStream.Publisher with EventStream.Subscriber

object EventStream {

  trait Event

  trait Publisher {
    def publishEvent(event: Event)(implicit context: ActorContext): Unit = {
      context.system.eventStream.publish(event)
    }
  }

  trait Subscriber {
    def subscribeEvent(actor: ActorRef, channel: Class[_ <: Event])(implicit
        context: ActorContext
    ): Unit = {
      require(context.system.eventStream.subscribe(actor, channel))
    }

    def unsubscribeEvent(actor: ActorRef, channel: Class[_ <: Event])(implicit
        context: ActorContext
    ): Unit = {
      require(context.system.eventStream.unsubscribe(actor, channel))
    }
  }
}
