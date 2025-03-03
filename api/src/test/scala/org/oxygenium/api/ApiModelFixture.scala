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

package org.oxygenium.api

import org.oxygenium.protocol._
import org.oxygenium.protocol.config._
import org.oxygenium.protocol.model._
import org.oxygenium.protocol.vm
import org.oxygenium.protocol.vm.LockupScript
import org.oxygenium.util._

trait ApiModelFixture
    extends ModelGenerators
    with ConsensusConfigsFixture.Default
    with NetworkConfigFixture.Default
    with ApiModelCodec {

  val instrs: AVector[vm.Instr[vm.StatefulContext]] =
    AVector(vm.ConstTrue, vm.ConstFalse, vm.I256Const3)
  val method  = vm.Method[vm.StatefulContext](true, true, true, true, 1, 2, 3, instrs)
  val methods = AVector(method, method)
  val script  = vm.StatefulScript.unsafe(methods)
  val assetTxOutputRef = AssetOutputRef.unsafe(
    Hint.unsafe(0),
    TxOutputRef.unsafeKey(hashGen.sample.get)
  )
  val contractTxOutputRef = ContractOutputRef.unsafe(
    Hint.unsafe(0),
    TxOutputRef.unsafeKey(hashGen.sample.get)
  )
  val (priKey, pubKey) = keypairGen.sample.get

  val sigature = SignatureSchema.sign(hashGen.sample.get.bytes, priKey)

  val scriptPair = p2pkScriptGen(GroupIndex.unsafe(0)).sample.get

  val txInput = TxInput(assetTxOutputRef, scriptPair.unlock)
  val assetOutput = TxOutput.asset(
    OXM.oneOxm,
    AVector.empty,
    LockupScript.p2pkh(Hash.zero)
  )

  val contractOutput = TxOutput.contract(
    OXM.oneOxm,
    LockupScript.p2c(ContractId.zero)
  )

  val unsignedTransaction = UnsignedTransaction(
    Some(script),
    minimalGas,
    nonCoinbaseMinGasPrice,
    AVector(txInput),
    AVector(assetOutput)
  )

  val transaction = Transaction(
    unsignedTransaction,
    scriptExecutionOk = true,
    AVector(contractTxOutputRef),
    AVector(assetOutput, contractOutput),
    AVector(sigature),
    AVector(sigature)
  )

  val transactionTemplate = TransactionTemplate(
    unsignedTransaction,
    AVector(sigature),
    AVector(sigature)
  )
}
