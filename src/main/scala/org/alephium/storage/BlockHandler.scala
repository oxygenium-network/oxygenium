package org.alephium.storage

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props}
import org.alephium.crypto.Keccak256
import org.alephium.network.PeerManager
import org.alephium.protocol.message.{Message, SendBlocks}
import org.alephium.protocol.model.Block
import org.alephium.storage.BlockFlow.ChainIndex
import org.alephium.util.BaseActor

object BlockHandler {

  def props(): Props = Props(new BlockHandler())

  sealed trait Command
  case class AddBlocks(blocks: Seq[Block])            extends Command
  case class GetBlocksAfter(locators: Seq[Keccak256]) extends Command
  case object GetBestHeader                           extends Command
  case object GetBestChain                            extends Command
  case object GetAllHeaders                           extends Command
  case object GetBlockInfo                            extends Command
  case class PrepareSync(remote: InetSocketAddress)   extends Command
  case class PrepareBlockFlow(chainIndex: ChainIndex) extends Command

  sealed trait Event
  case class BestHeader(header: Block)               extends Event
  case class BestChain(blocks: Seq[Block])           extends Event
  case class AllHeaders(headers: Seq[Keccak256])     extends Event
  case class BlockFlowTemplate(deps: Seq[Keccak256]) extends Event
}

// consider single chain for the moment
class BlockHandler() extends BaseActor {
  import BlockHandler._

  val blockFlow = BlockFlow()

  override def receive: Receive = awaitPeerManager

  def awaitPeerManager: Receive = {
    case PeerManager.Hello => context.become(handleWith(sender()))
  }

  def handleWith(peerManager: ActorRef): Receive = {
    case AddBlocks(blocks) =>
      // TODO: improve this
      require(blocks.nonEmpty && blocks.size == 1)
      val block = blocks.head
      if (blockFlow.contains(block)) {
        log.debug(s"Received already included block")
      } else {
        val ok = blockFlow.add(block)
        if (ok) {
          val index  = blockFlow.getIndex(block)
          val length = blockFlow.getBestLength
          val info   = blockFlow.getInfo
          log.debug(s"Add ${blocks.size} blocks for $index, #length: $length, info: $info")
          peerManager ! PeerManager.BroadCast(Message(SendBlocks(blocks)), sender())
        } else {
          log.warning(s"Failed to add a new block")
        }
      }
    case GetBlocksAfter(locators) =>
      val newBlocks = blockFlow.getBlocks(locators)
      sender() ! Message(SendBlocks(newBlocks))
    case GetBestHeader =>
      sender() ! BestHeader(blockFlow.getBestHeader)
    case GetBestChain =>
      sender() ! BestChain(blockFlow.getBestChain)
    case GetAllHeaders =>
      sender() ! AllHeaders(blockFlow.getAllHeaders)
    case GetBlockInfo =>
      sender() ! blockFlow.getBlockInfo
    case PrepareSync(remote: InetSocketAddress) =>
      // TODO: improve sync algorithm
      val headers = blockFlow.getAllHeaders
      sender() ! PeerManager.Sync(remote, headers)
    case PrepareBlockFlow(chainIndex) =>
      sender() ! BlockFlowTemplate(blockFlow.getBestDeps(chainIndex))
  }
}
