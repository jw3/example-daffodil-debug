daffodil debugger experiments
===

examples of extending the debugging capability in Daffodil

using unmodified Daffodil from master

### model

- Producers send [`Command[E]`](src/main/scala/ddb/debugger/package.scala)
```
trait Command[E <: Event] {
  def run(state: PState): ZIO[Any, Throwable, E]
}
```
- Sinks receive [`Event`](src/main/scala/ddb/debugger/package.scala), which are the product of a `Command` being applied to the processor state
```
trait Event
```
- Sinks and Producers are not connected

### reference
- https://github.com/apache/daffodil
- https://github.com/DFDLSchemas/JPEG
- https://github.com/apache/daffodil/pull/518
