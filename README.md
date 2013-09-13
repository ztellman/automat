![](docs/automat.JPG)

Automat is inspired by [Ragel](http://www.complang.org/ragel/).  However, instead of defining a DSL, it allows finite state automata to be built using simple composition of functions.

These automata, once compiled, are quite fast.  A single transition takes ~5ns, which allows around 200 million transitions a second.  This makes it appropriate for a wide variety of applications, including high-throughput parsing.

### usage

```clj
[automat "0.1.0-SNAPSHOT"]
```

A [finite-state automaton](http://en.wikipedia.org/wiki/Finite-state_machine) is defined as a series of states and transitions between these states, driven by a sequence of inputs. The automaton begins at a *start state*, and proceeds through the transitions until it reaches an *accept state*.  If given an input that isn't a valid transition, the automaton may either reject the input sequence or reset to the beginning, depending on the use case.

In Automat, an automata is defined as a chain of valid inputs:

```clj
> (require '[automat.viz :refer (view-fsm)])
nil
> (require '[automat.core :as a])
nil
> (view-graph (a/fsm 1 2 3))
```

![](docs/readme-0.png)



### license

Copyright © 2013 Zachary Tellman

Distributed under the MIT License
