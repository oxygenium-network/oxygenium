[View code on GitHub](https://github.com/oxygenium/oxygenium/protocol/src/main/scala/org/oxygenium/protocol/mining/Emission.scala)

The `Emission` class in the `org.oxygenium.protocol.mining` package is responsible for calculating the mining rewards for the Oxygenium blockchain. The class takes the block target time and group configuration as input parameters. It calculates the mining rewards based on the time, target, and hash rate of the mining process.

The class has several constants that define the initial and stable maximum rewards per chain, the low hash rate initial reward per chain, and the number of years until the rewards become stable and until there are no rewards. It also defines the number of blocks in about one year per chain, the blocks to stable max reward, and the blocks to no reward. It calculates the duration to stable max reward and the duration to no reward based on the block target time and the number of blocks.

The class has several methods that calculate the mining rewards based on the time, target, and hash rate of the mining process. The `rewardWrtTime` method calculates the mining reward based on the time elapsed since the launch of the blockchain. The `rewardWrtTarget` method calculates the mining reward based on the target of the mining process. The `rewardWrtHashRate` method calculates the mining reward based on the hash rate of the mining process.

The class also has a method that determines whether to enable the Proof of Linear Work (PoLW) based on the target of the mining process. The `shouldEnablePoLW` method returns true if the target is less than one EH/s.

The class has a `RewardType` trait that defines two case classes: `PoW` and `PoLW`. The `PoW` case class represents the mining reward for Proof of Work (PoW) mining, and the `PoLW` case class represents the mining reward for PoLW mining. The `PoLW` case class also includes the amount to burn, which is calculated based on the mining reward and the target of the mining process.

The class has two methods that calculate the rewards per year based on time and target. The `rewardsWrtTime` method calculates the rewards per year based on the time elapsed since the launch of the blockchain. The `rewardsWrtTarget` method calculates the rewards per year based on the hash rate of the mining process.

Overall, the `Emission` class is an essential part of the Oxygenium blockchain that calculates the mining rewards based on various parameters. It is used to incentivize miners to participate in the mining process and maintain the security of the blockchain.
## Questions: 
 1. What is the purpose of the `Emission` class?
- The `Emission` class is responsible for calculating mining rewards for the Oxygenium blockchain based on various factors such as time, hashrate, and target.

2. What is the significance of the `blockTargetTime` parameter?
- The `blockTargetTime` parameter represents the target time for generating a new block in the Oxygenium blockchain. It is used in various calculations to determine mining rewards.

3. What is the difference between `PoW` and `PoLW` in the `RewardType` trait?
- `PoW` represents a mining reward for proof-of-work mining, while `PoLW` represents a mining reward for proof-of-work and proof-of-low-work combined mining. The `PoLW` reward includes a burnt amount that is calculated based on the difference between the target and the `oneEhPerSecondTarget`.