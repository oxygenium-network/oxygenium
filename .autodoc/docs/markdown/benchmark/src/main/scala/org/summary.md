[View code on GitHub](https://github.com/oxygenium/oxygenium/.autodoc/docs/json/benchmark/src/main/scala/org)

The `.autodoc/docs/json/benchmark/src/main/scala/org/oxygenium/benchmark` folder contains various benchmarking tools for the Oxygenium project. These tools help developers measure the performance of different components and algorithms, identify bottlenecks, and optimize the codebase.

For example, `BlockFlowBench.scala` measures the average time it takes to execute the `findBestDeps()` method, which calculates the best dependencies for a given group index in the blockchain. This benchmark helps evaluate the performance of the `BlockFlow` class, which manages the blockchain data in Oxygenium.

```scala
val blockFlowBench = new BlockFlowBench()
val bestDeps = blockFlowBench.findBestDeps()
println(s"Best dependencies found: $bestDeps")
```

`CollectionBench.scala` compares the performance of two collection types, `Vector` and `AVector`, for various operations like appending elements, mapping, filtering, and flat-mapping. This benchmark helps developers choose the most efficient collection type for their use case.

```scala
val collectionBench = new CollectionBench()
val appendVectorTime = collectionBench.appendVector()
println(s"Appending to Vector took: $appendVectorTime ms")
```

`CryptoBench.scala` measures the performance of cryptographic hash functions and digital signature algorithms, such as `Blake2b`, `Keccak256`, `Sha256`, `Blake3`, and `SecP256K1`. This benchmark helps developers compare the performance of different cryptographic algorithms and choose the most suitable one for their needs.

```scala
val cryptoBench = new CryptoBench()
val blake2bTime = cryptoBench.blake2b()
println(s"Blake2b hashing took: $blake2bTime ms")
```

`MiningBench.scala` measures the throughput of mining a genesis block in the Oxygenium blockchain, testing the performance of the Proof of Work (PoW) algorithm. This benchmark helps developers determine the optimal hardware requirements for mining Oxygenium blocks and compare the PoW algorithm's performance to other blockchain platforms.

```scala
val miningBench = new MiningBench()
val result = miningBench.mineGenesis()
println(s"Genesis block mined: $result")
```

`RocksDBBench.scala` tests the performance of RocksDB, a high-performance embedded database for key-value data, under different settings like compaction strategies and memory budgets. This benchmark helps developers determine the optimal settings for a RocksDB database based on its performance under different conditions.

```scala
val rocksDBBench = new RocksDBBench()
val ssdSettingsTime = rocksDBBench.ssdSettings()
println(s"RocksDB with SSD settings took: $ssdSettingsTime ms")
```

`TrieBench.scala` measures the performance of the `SparseMerkleTrie` data structure, which stores key-value pairs in a tree-like structure for efficient lookups and updates. This benchmark helps developers optimize the performance of the trie for use in the larger project.

```scala
val trieBench = new TrieBench()
val randomInsertTime = trieBench.randomInsert()
println(s"Random insertions took: $randomInsertTime ms")
```

`TxOrderBench.scala` measures the average time it takes to calculate the execution order of transactions in a block. This benchmark helps developers optimize the execution order of transactions, improving the overall performance and scalability of the blockchain network.

```scala
val txOrderBench = new TxOrderBench()
val calculateRandomOrderTime = txOrderBench.calculateRandomOrder()
println(s"Calculating random order took: $calculateRandomOrderTime ms")
```
