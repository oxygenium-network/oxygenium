[View code on GitHub](https://github.com/oxygenium/oxygenium/api/src/main/scala/org/oxygenium/api/model/ContractState.scala)

This file contains two case classes, `ContractState` and `AssetState`, and an object `AssetState` with two methods. These classes and methods are used to represent and manipulate the state of a smart contract in the Oxygenium blockchain.

`ContractState` represents the state of a smart contract and contains the following fields:
- `address`: the address of the contract
- `bytecode`: the bytecode of the contract
- `codeHash`: the hash of the contract's bytecode
- `initialStateHash`: an optional hash of the initial state of the contract
- `immFields`: a vector of immutable fields of the contract
- `mutFields`: a vector of mutable fields of the contract
- `asset`: an `AssetState` object representing the assets held by the contract

`AssetState` represents the assets held by a smart contract and contains the following fields:
- `attoOxmAmount`: the amount of Oxygenium tokens held by the contract
- `tokens`: an optional vector of tokens held by the contract

`AssetState` also contains two methods:
- `from(attoOxmAmount: U256, tokens: AVector[Token]): AssetState`: creates an `AssetState` object from the given amount of Oxygenium tokens and vector of tokens
- `from(output: ContractOutput): AssetState`: creates an `AssetState` object from the given `ContractOutput` object

`ContractState` has a method `id` that returns the ID of the contract, which is derived from the contract's lockup script.

These classes and methods are used throughout the Oxygenium project to represent and manipulate the state of smart contracts. For example, `ContractState` objects are used to store the state of contracts in the blockchain, and `AssetState` objects are used to represent the assets held by contracts. The `AssetState` methods are used to create and manipulate `AssetState` objects from `ContractOutput` objects, which are used to represent the output of a contract execution. Overall, these classes and methods are an important part of the Oxygenium blockchain's smart contract functionality.
## Questions: 
 1. What is the purpose of the `oxygenium.api.model` package?
- The `oxygenium.api.model` package contains classes that represent data models used in the Oxygenium API.

2. What is the `AssetState` case class used for?
- The `AssetState` case class represents the state of an asset, including the amount of attoOxm and an optional vector of tokens.

3. What is the `toContractOutput` method used for in the `AssetState` case class?
- The `toContractOutput` method is used to convert an `AssetState` object to a `ContractOutput` object, which is used to represent the output of a contract.