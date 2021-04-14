daffodil debugger experiments
===

Examples of extending the debugging capability in Daffodil using a new protocol that follows in the footsteps of the Daffodil TUI debugger.

Uses a snapshot from the current Daffodil master branch.

### examples
- [Custom Daffodil Debugger using Zio](src/main/scala/ddb/debugger/z)
  - simulates a stepping control that injects a step command that moves the debugger to the next step
  - stateless views
    - display infoset at the current step
    - display the current bit position in the data and the corresponding value
  - stateful views
    - display the diff of current infoset against previous step
  - integrates scalafx gui to inject manual control and feedback

### model

Commands applied to the current ParseState produce Events.

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

### it is what it is

![](doc/img.png)

### reference
- https://github.com/apache/daffodil
- https://github.com/DFDLSchemas/JPEG
- https://github.com/apache/daffodil/pull/518
