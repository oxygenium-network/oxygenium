[View code on GitHub](https://github.com/oxygenium/oxygenium/protocol/src/main/scala/org/oxygenium/protocol/vm/MutBalancesPerLockup.scala)

This file contains the implementation of the `MutBalancesPerLockup` class, which represents the mutable balances of a lockup script. A lockup script is a smart contract that locks up some funds and/or tokens for a certain period of time. The purpose of this class is to keep track of the balances of the lockup script during its execution.

The `MutBalancesPerLockup` class has three fields: `attoOxmAmount`, `tokenAmounts`, and `scopeDepth`. `attoOxmAmount` is an instance of the `U256` class, which represents a 256-bit unsigned integer. It represents the balance of the lockup script in the Oxygenium currency (OXM). `tokenAmounts` is a mutable map that maps token IDs to their balances. `scopeDepth` is an integer that represents the depth of the current scope in the lockup script.

The class provides several methods to manipulate the balances. The `addOxm` method adds a given amount of OXM to the balance. The `addToken` method adds a given amount of a given token to the balance. The `subOxm` and `subToken` methods subtract a given amount of OXM or a given token from the balance, respectively. The `add` and `sub` methods add or subtract the balances of another `MutBalancesPerLockup` instance to/from this instance.

The class also provides several methods to convert the balances to transaction outputs. The `toTxOutput` method converts the balances to a vector of transaction outputs, depending on the type of the lockup script and the state of the hard fork. The `toTxOutputLeman` and `toTxOutputDeprecated` methods are helper methods that implement the conversion logic for the Leman hard fork and the pre-Leman hard fork, respectively. The `toLockedTxOutput` method is a convenience method that converts the balances to a transaction output for an asset lockup script with a given lock time.

The `MutBalancesPerLockup` class is used in the larger Oxygenium project to implement the logic of lockup scripts. It provides a convenient way to keep track of the balances of a lockup script during its execution and to convert the balances to transaction outputs. The class is mutable, which allows for efficient updates of the balances during the execution of the lockup script.
## Questions: 
 1. What is the purpose of this code?
- This code defines a case class `MutBalancesPerLockup` and companion object with methods for adding, subtracting, and converting balances of Oxmium and tokens.

2. What is the license for this code?
- This code is licensed under the GNU Lesser General Public License version 3 or later.

3. What other packages or classes does this code depend on?
- This code depends on several other packages and classes, including `org.oxygenium.protocol.model`, `org.oxygenium.util`, and `scala.collection.mutable`.