[View code on GitHub](https://github.com/oxygenium/oxygenium/.autodoc/docs/json/rpc)

The `.autodoc/docs/json/rpc` folder contains the documentation for the JSON-RPC 2.0 implementation in the Oxygenium project. JSON-RPC is a remote procedure call (RPC) protocol encoded in JSON, providing a standardized way for clients to interact with the Oxygenium node.

The implementation is located in the `src` subfolder, within the `org.oxygenium.rpc.model` package. The `JsonRPC` object defines several case classes and traits that represent JSON-RPC requests, notifications, and responses:

- `Request`: Represents a JSON-RPC request containing the method name, parameters, and an ID.
- `Response`: Represents a JSON-RPC response, which can be either a `Success` or a `Failure`, containing either a result or an error, respectively.
- `Handler`: A map of method names to functions that take a `Request` object and return a `Future` of a `Response`.

The `JsonRPC` object also provides helper methods for working with JSON objects, such as `paramsCheck` for validating parameter objects and `versionSet` for adding the JSON-RPC version to a JSON object.

Here's an example of how the `JsonRPC` object might be used in the Oxygenium project:

```scala
import org.oxygenium.rpc.model.JsonRPC

// Define a handler function for the "echo" method
val handler: JsonRPC.Handler = Map(
  "echo" -> { request =>
    val params = request.paramsAs[String]
    params match {
      case Right(str) => JsonRPC.Response.successful(request, str)
      case Left(failure) => failure
    }
  }
)

// Parse a JSON-RPC request and run it with the handler
val requestJson = """{"jsonrpc": "2.0", "method": "echo", "params": "hello", "id": 1}"""
val request = upickle.default.read[JsonRPC.RequestUnsafe](requestJson)
val response = request.runWith(handler)

// Serialize the response to JSON
val responseJson = upickle.default.write(response)
```

In this example, we define a handler function for the "echo" method, which simply returns the input string. We then parse a JSON-RPC request, run it with the handler, and serialize the response to JSON.

This implementation of JSON-RPC allows the Oxygenium project to provide a consistent and standardized interface for clients to interact with the Oxygenium node. Clients can send JSON-RPC requests to the node, which are then handled by the `Handler` functions defined in the `JsonRPC` object. The `JsonRPC` object is responsible for parsing the requests, validating them, and returning the appropriate response.
