[View code on GitHub](https://github.com/oxygenium/oxygenium/flow/src/main/scala/org/oxygenium/flow/Utils.scala)

The code defines a set of utility functions that can be used across the Oxygenium project. These functions are designed to help with displaying various types of data in a human-readable format. 

The `showDigest` function takes a vector of `RandomBytes` objects and returns a string representation of the vector. If the vector is empty, it returns `"[]"`. Otherwise, it returns a string containing the short hexadecimal representation of the first and last elements of the vector, separated by `".."` and enclosed in square brackets. For example, `showDigest(AVector(RandomBytes(1), RandomBytes(2), RandomBytes(3)))` would return `"[ 01..03 ]"`.

The `showTxs` function takes a vector of `TransactionTemplate` objects and returns a string representation of the vector. If the vector is empty, it returns `"[]"`. Otherwise, it returns a string containing the short hexadecimal representation of the IDs of the first and last transactions in the vector, separated by `".."` and enclosed in square brackets. For example, `showTxs(AVector(TransactionTemplate(TransactionId(1)), TransactionTemplate(TransactionId(2)), TransactionTemplate(TransactionId(3))))` would return `"[ 01..03 ]"`.

The `showFlow` function takes a vector of vectors of `RandomBytes` objects and returns a string representation of the vector. It does this by mapping the `showDigest` function over each inner vector and then joining the resulting strings with `", "`. For example, `showFlow(AVector(AVector(RandomBytes(1), RandomBytes(2)), AVector(RandomBytes(3), RandomBytes(4)))))` would return `"[ [ 01..02 ], [ 03..04 ] ]"`.

The `showDataDigest` function takes a vector of `FlowData` objects and returns a string representation of the vector. If the vector is empty, it returns `"[]"`. Otherwise, it returns a string containing the short hexadecimal representation of the first and last elements of the vector, separated by `".."` and enclosed in square brackets. For example, `showDataDigest(AVector(FlowData(1), FlowData(2), FlowData(3)))` would return `"[ 01..03 ]"`.

The `showChainIndexedDigest` function takes a vector of pairs of `ChainIndex` and vectors of `TransactionId` objects and returns a string representation of the vector. It does this by mapping a function that combines the `ChainIndex` and the result of calling `showDigest` on the corresponding vector of `TransactionId` objects over each pair and then joining the resulting strings with `", "`. For example, `showChainIndexedDigest(AVector((ChainIndex(1), AVector(TransactionId(1), TransactionId(2))), (ChainIndex(2), AVector(TransactionId(3), TransactionId(4))))` would return `"[ 1 -> [ 01..02 ], 2 -> [ 03..04 ] ]"`.

The `unsafe` function takes an `IOResult` object and returns the value contained in it if it is a `Right` object. If it is a `Left` object, it throws the exception contained in it. This function is used to extract the value from an `IOResult` object when it is known that the operation that produced the object will not fail.
## Questions: 
 1. What is the purpose of this code file?
- This code file contains utility functions for displaying various types of data in a specific format.

2. What external libraries or dependencies does this code use?
- This code imports several classes from other packages within the oxygenium project, as well as the `RandomBytes` class from the `org.oxygenium.serde` package.

3. What is the license for this code?
- This code is released under the GNU Lesser General Public License, version 3 or later.