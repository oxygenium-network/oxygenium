[View code on GitHub](https://github.com/oxygenium/oxygenium/flow/src/main/scala/org/oxygenium/flow/setting/OxygeniumConfig.scala)

This code defines the configuration settings for the Oxygenium project, a blockchain platform. The configuration settings are organized into several case classes, each representing a specific aspect of the system, such as consensus, mining, network, discovery, mempool, wallet, node, and genesis settings.

For example, the `ConsensusSetting` case class contains settings related to the consensus algorithm, such as block target time, uncle dependency gap time, and the number of zeros required in the hash. Similarly, the `MiningSetting` case class contains settings related to mining, such as miner addresses, nonce step, and batch delay.

The `OxygeniumConfig` case class combines all these settings into a single configuration object, which can be loaded from a configuration file using the `load` method. This method takes an environment, a root path, and a configuration path as input and returns an `OxygeniumConfig` object with the parsed settings.

The code also provides sanity checks for the configuration settings, such as ensuring that the timestamp for the leman hard fork is valid for the Oxygenium MainNet.

Here's an example of how to load the configuration settings:

```scala
val configPath = "path/to/config/file"
val rootPath = Paths.get("path/to/root")
val oxygeniumConfig = OxygeniumConfig.load(rootPath, configPath)
```

This configuration object can then be used throughout the Oxygenium project to access various settings and customize the behavior of the system.
## Questions: 
 1. **Question**: What is the purpose of the `OxygeniumConfig` object and its related case classes?
   **Answer**: The `OxygeniumConfig` object and its related case classes are used to define and load the configuration settings for the Oxygenium project. These settings include broker, consensus, mining, network, discovery, mempool, wallet, node, and genesis configurations.

2. **Question**: How does the `load` method work in the `OxygeniumConfig` object?
   **Answer**: The `load` method in the `OxygeniumConfig` object is used to load the configuration settings from a given `Config` object and an optional `configPath`. It first extracts the configuration settings using the `oxygeniumValueReader` and then performs a sanity check on the loaded configuration before returning it.

3. **Question**: What is the purpose of the `sanityCheck` method in the `OxygeniumConfig` object?
   **Answer**: The `sanityCheck` method is used to validate the loaded configuration settings, specifically checking if the `networkId` is set to `OxygeniumMainNet` and if the `lemanHardForkTimestamp` has the correct value. If the check fails, an `IllegalArgumentException` is thrown.