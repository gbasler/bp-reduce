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

## Example Run

- if you haven't, compile via `sbt tc` (get and install sbt from [here](http://www.scala-sbt.org/)).
- run bp-reduce from IntelliJ or withn sbt with the example file from the repo:
`sbt bp-reduce-core/runMain bpReduce.main.Main """C:\code\bp-reduce\bp-reduce-core\src\test\resources\trace_WP_bug2\main.bp"""`

Then the magic starts:
```
reduced.1.bp: accepted in 11340.179 ms.
reduced.2.bp: accepted in 10759.167 ms.
reduced.3.bp: accepted in 10611.194 ms.
reduced.4.bp: rejected in 18818.037 ms.
reduced.5.bp: rejected in 1678.041 ms.
reduced.6.bp: accepted in 10478.264 ms.
reduced.7.bp: accepted in 10459.801 ms.
reduced.8.bp: rejected in 64.464 ms.
...
```

Until the search terminates (after ~30 mins) and you'll get a `reduced.bp` file:

```
decl b0_s_le_2;
decl b1;
decl b2;
void main() begin
	decl b3_l_eq_s;
	decl b4_0_eq_l;
	decl b5_1_eq_l;
PC2:	b2 := F;
l1: PC10:	skip;
PC11:	start_thread goto l5;
PC13:	b3_l_eq_s := *;
PC18:	if F then goto l2; fi;
PC19:	if !b2 then goto l2; fi;
PC22:	assert F;
l2: PC23:	skip;
l3: PC31:	b3_l_eq_s, b3_l_eq_s$ := F, b3_l_eq_s;
PC36:	b1, b2 := T, T;
PC38:	end_thread;
l5: PC39:	goto l1;
end
```

This file is signifficantly shorter than the original file (I omitted the dead functions for brevity, this is not
implemented yet).

Judging from the output, it seems that there's a bug
in the handling of mixed variables.

So from a 200 lines failing regression test we got to a 20 lines regression,
that's a reduction by 10x!
Not too bad for a fully automated search!

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
Copyright (c) 2013, 2014 Gérard Basler

Published under the [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0.txt)