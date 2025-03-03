[View code on GitHub](https://github.com/oxygenium/oxygenium/flow/src/main/scala/org/oxygenium/flow/core/UtxoSelectionAlgo.scala)

The `UtxoSelectionAlgo` object in this code is responsible for selecting unspent transaction outputs (UTXOs) to be used as inputs for a new transaction. The selection process aims to satisfy the required amounts of OXM (the native token) and other tokens while considering the gas fees for the transaction. The algorithm supports two selection orders: ascending and descending, based on the amount and type of UTXOs.

The `select` method in the `Build` case class is the main entry point for the UTXO selection process. It first tries to select UTXOs using the ascending order. If it fails, it falls back to the descending order. The selection process is divided into two parts: without gas estimation and with gas estimation.

The `SelectionWithoutGasEstimation` case class handles the selection of UTXOs without considering gas fees. It first selects UTXOs for tokens and then for OXM. The `selectForAmount` method is a helper function that selects UTXOs based on the required amount and a given ordering.

The `SelectionWithGasEstimation` case class handles the selection of UTXOs while considering gas fees. It iterates through the remaining UTXOs and estimates the gas fees for each UTXO. If the sum of the selected UTXOs and the gas fees is greater than or equal to the required amount, the selection process is successful.

Example usage:

```scala
val selectedUtxos = UtxoSelectionAlgo.Build(providedGas).select(
  amounts,
  unlockScript,
  utxos,
  txOutputsLength,
  txScriptOpt,
  assetScriptGasEstimator,
  txScriptGasEstimator
)
```

In this example, `selectedUtxos` will be an instance of `Either[String, Selected]`, where `Selected` contains the selected UTXOs and the gas fees for the transaction.
## Questions: 
 1. **Question**: What is the purpose of the `UtxoSelectionAlgo` object in this code?
   **Answer**: The `UtxoSelectionAlgo` object is responsible for selecting unspent transaction outputs (UTXOs) based on certain criteria such as amount, type, and priority. It provides various algorithms for selecting UTXOs for a transaction, considering factors like gas estimation, token amounts, and asset order.

2. **Question**: How does the code handle the selection of UTXOs with different asset orders?
   **Answer**: The code provides two asset order implementations: `AssetAscendingOrder` and `AssetDescendingOrder`. These implementations define the ordering of assets based on their amount, type, and priority. The `BuildWithOrder` case class takes an `AssetOrder` as a parameter and uses it to sort and select UTXOs accordingly.

3. **Question**: How does the code handle gas estimation for transactions?
   **Answer**: The code handles gas estimation through the `GasEstimation` object and its methods. It estimates gas for both asset scripts and transaction scripts, considering factors like input scripts, output scripts, and the number of inputs and outputs. The `Build` and `BuildWithOrder` case classes use these gas estimations to select UTXOs that can cover the required gas fees.