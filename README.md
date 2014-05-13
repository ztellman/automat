![](docs/automat.JPG)

Automat is a library for defining and using finite-state automata, inspired by [Ragel](http://www.complang.org/ragel/).  However, instead of defining a DSL, it allows them to be built using simple composition of functions.

These automata, once compiled, are quite fast.  An array with 100 million elements can be processed in 500ms, giving a mean transition time of 5ns.  However, Automat isn't just for high throughput use cases; it's meant to be useful wherever an FSM is necessary.

```clj
[automat "0.1.0-SNAPSHOT"]
```

### defining an FSM

A [finite-state machine](http://en.wikipedia.org/wiki/Finite-state_machine) or finite-state automaton is defined as a series of states and transitions between these states, driven by a sequence of inputs. The automaton begins at a **start state**, and proceeds through the transitions until it reaches an **accept state**.  If given an input that isn't a valid transition, the automaton may either reject the input sequence or reset to the beginning, depending on the use case.

In Automat, the simplest automaton is simply a vector representing a chain of valid inputs.

```clj
> (require '[automat.viz :refer (view)])
nil
> (require '[automat.core :as a])
nil
> (view [1 2 3])
```

![](docs/readme-0.png)

The circle on the left is the start state, and the circle on the right with the double-lined border is the accept state.  Note that the transitions don't have to be numbers:

```clj
> (view [:foo "bar" 'baz])
```

![](docs/readme-1.png)

Each argument to `fsm` can either be an input or another automaton.

```clj
> (view [1 [2 [3]]])
```

![](docs/readme-0.png)

Note that this is identical to the first automaton.  We can also combine existing automatons using the operators in `automat.core`:

```clj
> (view (a/or [1 2 3] [1 3]))
```

![](docs/readme-2.png)

This represents the **union** of the two automata, and returns an automaton which will either accept `1, 2, 3` or `1, 3`.

If we want to accept a range of inputs, we can use `..`:

```clj
> (view [1 (a/.. 2 10) 11])
```

![](docs/readme-3.png)

This will accept `1, 2, 11`, `1, 3, 11`, and so on.  If we subsequently want to narrow this, we can use `and`:

```clj
> (view
    (a/and
      [1 (a/.. 2 10) 11]
      (a/or
        [1 2 11]
        [1 7 11])))
```

![](docs/readme-4.png)

This represents the **intersection** of two automata, in this case giving us an automaton that either accepts `1, 2, 11` or `1, 7, 11`.  Note that if the intersection is empty, this will give us an automaton that cannot accept anything.

```clj
> (view (a/difference (a/.. 1 10) 2 (a/.. 5 6)))
```

![](docs/readme-7.png)

This represents the **difference** between the automata, in this case an automata that accepts `[1,10]`, less the inputs `2, 5, 6`.

The operators `*`, `+`, and `?` behave as they do in regular expressions:

```clj
> (view [(a/? 1) (a/* 2) (a/+ 3)])
```

This gives us an automaton that accepts zero or one `1` inputs, zero or more `2` inputs, and one or more `3` inputs.

![](docs/readme-5.png)

The `not` operator is equivalent to the regex `^` operator:

```clj
> (view [1 (a/not 2) 3])
```

![](docs/readme-6.png)

In this diagram, `DEF` represents the default transition (in this case, anything but `2`), and `REJ` represents a rejection state.

### using an FSM

Once we've defined an FSM, we can `compile` it:

```clj
(a/compile [1 2 3])
```

This will optimize the FSM, emit code that processes it, and call `eval` on it.  The resulting compiled FSM can be interacted with via `find` and `greedy-find`.  Each function takes a compiled FSM, an FSM state, and a stream of inputs.

An FSM state can be created using `(a/start compiled-fsm value)`:

```clj
> (def f (a/compile [1 2 3]))
#'f
> (a/start f nil)
{:accepted? false, :checkpoint nil, :state-index 0, :start-index 0, :stream-index 0, :value nil}
```

| field | description |
|-------|-------------|
| `accepted?` | whether the FSM is at an accept state |
| `state-index` | the numeric identifier for the current FSM state |
| `start-index` | the stream index where the FSM last (re)started |
| `stream-index` | the current stream index |
| `checkpoint` | the previous match, only non-nil when used with `greedy-find` |
| `value` | the current reduce value, explained below |

`find` works similarly to regex matching.  It takes the stream of inputs, which can be a byte-array, `java.nio.Buffer`, `java.io.InputStream`, `java.io.Reader`, or a normal Clojure sequence. The inputs in the FSM correspond to elements from these streams, which will be consumed until an accept state is reached, or the end of the stream is reached.  In either case, `find` will return a new state.

If a match is found, `accepted?` will be true, `start-index` will be the point within the stream where the match begins, and `stream-index` will be the point where the match ends.

```clj
> (a/find f (a/start f nil) [1 2 3])
{:accepted? true, :checkpoint nil, :state-index 3, :start-index 0, :stream-index 3, :value nil}
```

If `accepted?` is false, the `start-index` will describe where the FSM last restarted.  This state can be passed into `find` with a new stream of inputs.

```clj
> (a/find f (a/start f nil) [1 2 1 2])
{:accepted? false, :checkpoint nil, :state-index 2, :start-index 2, :stream-index 4, :value nil)}
```

In either case, `state-index` will describe where we are in the actual compiled automaton.  This can be correlated by calling `automat.viz/view` on a compiled FSM:

```clj
> (view (a/compile [4 5 6]))
```

![](docs/readme-8.png)

Here, each node is labelled with the corresponding `state-index`.

This is useful for identifying the beginning and end of patterns within a sequence, but if we've joined multiple patterns together with an `or`, or simply just don't want to process the sub-sequence a second time, we may wish to treat the FSM processing as a reduction over the input.

To do this, we use the `$` operator, which represents a reduce action between state transitions.  Let's consider the simple case, where we want to accumulate the inputs which we've matched:

```clj
> (def fsm [($ :clear) 1 ($ :conj) 2 ($ :conj)])
```

When we compile, we can specify functions that correpsond to these action keywords:

```clj
> (def f (compile fsm {:clear (constantly []), :conj conj}))
```

Now, when we call `find`, we will make use of `value` in `(start fsm value)`, which specifies the initial value that will be passed through our reducer functions:

```clj
> (find f (start f nil]) [1 1 2])
{:accepted? true, :checkpoint nil, :state-index 2, :start-index 2, :stream-index 4, :value [1 2]}
```

Because we've specified an action before our first input, we simply define the initial value as `nil`, but other initial values may be useful, depending on the situation.  We also may not wish to manually specify an action between each input.  For this, we can use `interleave-$`:

```clj
> (def fsm (list* ($ :clear) (range 5)))
#'fsm
> (def f
    (interleave-$ fsm :conj)
    {:clear (constantly [])
     :conj conj})
#'f
> (find f (start f nil) (range 5))
{:accepted? true, :checkpoint nil, :state-index 5, :start-index 0, :stream-index 5, :value [0 1 2 3 4]}
```

Multiple actions can be placed between each input, as long as their order of execution doesn't matter.  Using `$` can be a powerful tool, but it will have performance impacts - for high throughput use cases prefer using the `:start-index` and `:stream-index` values to pull out the input data in bulk.

When defining patterns that are subsets of each other, such as `(or [1 2] [1 2 3])`, `find` will always return upon matching the shortest of the two.  If we want to match on the longest, we can use `greedy-find` instead.  Note that `greedy-find` will always consume at least one more input than it needs for the match, which means that reduce actions used with it must not have side effects.

### license

Copyright © 2014 Zachary Tellman

Distributed under the MIT License
