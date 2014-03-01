#BP-Reduce [![Build Status](https://travis-ci.org/gbasler/bp-reduce.png?branch=master)](https://travis-ci.org/gbasler/bp-reduce)

## Motivation

Test cases must be as small as possible such that it's easy to fix the bug found and it can be collected
as a regression test afterwards.
Reducing test cases by hand is tedious and boring. Writing a tool on the other hand that does the job is fun!
This project is inspired by:
[C-Reduce](http://embed.cs.utah.edu/creduce/).

## How it works

* Parse Boolean program and build up AST
* Trial and error: reduce program (on AST) and check if it still has the same effect on the Model Checker
(crash, wrong verification error and so on). If so, try to reduce more. If not undo reduction and try a different one.
Important: the tool should only generate valid programs that pass syntactic and semantic check.

## The C-Reduce algorithm

```
current = original_test_case
while (!fixpoint) {
    foreach t in transformations {
        state = t::new ()
        while (true) {
            variant = current
            result = t::transform (variant, state)
            if (result == stop)
                break
            /* variant has behavior of interest
            and meets validity criterion? */
            if (is_successful (variant))
                current = variant
            else
                state = t::advance (current, state)
        }
    }
}
```

 * All transformations are applied in sequence until no transformation is able to reduce the test case anymore (fixpoint)
 * A transformation must implement three functions:
   * `new`, takes no parameters and returns a fresh transformation state
   * `transform`, takes a state object and a path to a test case; it modiﬁes the test case in place and returns a status code that is either:
     * `ok`, indicating that a transformation was performed; or
     * `stop`, indicating that the transformation has run out of opportunities for the test case at hand.
   * `advance`, takes a state object and a path to a test case; it advances the iterator to the next transformation opportunity 

### Possible reductions

All these reductions expect that the program has already been simplified before
(no skip statements, gotos linearized etc).

 * eliminate dead code
   * set variables to constant true / false or eliminate completely (_implemented_)
   * remove statements (_implemented_)
 * simplify expressions in assume / if / assignment (_implemented_)
   * replace operand with true / false (_implemented_)
 * rename variables to single case letters

### Some ideas for improvement

- add artificial `x := *` assignments at the beginning of the program, and try to reduce them to `x := T`, `x := F`
in order to reduce the state space
- come up with some kind of binary search or grouping of expression / stmt reductions, if the basic reduction works,
jump in the middle of the lattice and check there

## License
Copyright (c) 2013 Gérard Basler

Published under the [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0.txt)