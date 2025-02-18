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

package org.oxygenium.flow.core

import org.oxygenium.ralph.Compiler

object AMMContract {
  lazy val swapContract =
    s"""
       |// Simple swap contract purely for testing
       |
       |Contract Swap(tokenId: ByteVec, mut alphReserve: U256, mut tokenReserve: U256) {
       |  event AddLiquidity(lp: Address, attoOxmAmount: U256, tokenAmount: U256)
       |  event SwapToken(buyer: Address, attoOxmAmount: U256)
       |  event SwapOxm(buyer: Address, tokenAmount: U256)
       |
       |  @using(preapprovedAssets = true, assetsInContract = true, updateFields = true)
       |  pub fn addLiquidity(lp: Address, attoOxmAmount: U256, tokenAmount: U256) -> () {
       |    emit AddLiquidity(lp, attoOxmAmount, tokenAmount)
       |
       |    transferTokenToSelf!(lp, OXM, attoOxmAmount)
       |    transferTokenToSelf!(lp, tokenId, tokenAmount)
       |    alphReserve = alphReserve + attoOxmAmount
       |    tokenReserve = tokenReserve + tokenAmount
       |  }
       |
       |  @using(preapprovedAssets = true, assetsInContract = true, updateFields = true)
       |  pub fn swapToken(buyer: Address, attoOxmAmount: U256) -> () {
       |    emit SwapToken(buyer, attoOxmAmount)
       |
       |    let tokenAmount = tokenReserve - alphReserve * tokenReserve / (alphReserve + attoOxmAmount)
       |    transferTokenToSelf!(buyer, OXM, attoOxmAmount)
       |    transferTokenFromSelf!(buyer, tokenId, tokenAmount)
       |    alphReserve = alphReserve + attoOxmAmount
       |    tokenReserve = tokenReserve - tokenAmount
       |  }
       |
       |  @using(preapprovedAssets = true, assetsInContract = true, updateFields = true)
       |  pub fn swapOxm(buyer: Address, tokenAmount: U256) -> () {
       |    emit SwapOxm(buyer, tokenAmount)
       |
       |    let attoOxmAmount = alphReserve - alphReserve * tokenReserve / (tokenReserve + tokenAmount)
       |    transferTokenToSelf!(buyer, tokenId, tokenAmount)
       |    transferTokenFromSelf!(buyer, OXM, attoOxmAmount)
       |    alphReserve = alphReserve - attoOxmAmount
       |    tokenReserve = tokenReserve + tokenAmount
       |  }
       |}
       |""".stripMargin
  lazy val swapCode = Compiler.compileContract(swapContract).toOption.get

  lazy val swapProxyContract: String =
    s"""
       |Contract SwapProxy(swapContract: Swap, tokenId: ByteVec) {
       |  @using(preapprovedAssets = true)
       |  pub fn addLiquidity(lp: Address, attoOxmAmount: U256, tokenAmount: U256) -> () {
       |    swapContract.addLiquidity{
       |      lp -> OXM: attoOxmAmount, tokenId: tokenAmount
       |    }(lp, attoOxmAmount, tokenAmount)
       |  }
       |
       |  @using(preapprovedAssets = true)
       |  pub fn swapToken(buyer: Address, attoOxmAmount: U256) -> () {
       |    swapContract.swapToken{buyer -> OXM: attoOxmAmount}(buyer, attoOxmAmount)
       |  }
       |
       |  @using(preapprovedAssets = true)
       |  pub fn swapOxm(buyer: Address, tokenAmount: U256) -> () {
       |    swapContract.swapOxm{buyer -> tokenId: tokenAmount}(buyer, tokenAmount)
       |  }
       |}
       |
       |$swapContract
       |""".stripMargin
  lazy val swapProxyCode = Compiler.compileContract(swapProxyContract).toOption.get
}
