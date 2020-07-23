package org.alephium.protocol.model

import org.scalacheck.Gen

import org.alephium.protocol.ALF.Hash
import org.alephium.protocol.config.ConsensusConfigFixture
import org.alephium.util.AlephiumSpec

class TransactionSpec
    extends AlephiumSpec
    with ConsensusConfigFixture
    with NoIndexModelGeneratorsLike {
  it should "generate distinct coinbase transactions" in {
    val (_, key)    = GroupIndex.unsafe(0).generateKey
    val coinbaseTxs = (0 to 1000).map(_ => Transaction.coinbase(key, 0, Hash.generate.bytes))

    coinbaseTxs.size is coinbaseTxs.distinct.size
  }

  it should "calculate chain index" in {
    forAll(chainIndexGen) { chainIndex =>
      forAll(transactionGen()(chainIndexGen = Gen.const(chainIndex))) { tx =>
        tx.chainIndex is chainIndex
      }
    }
  }
}
