# SACK: Semantic Automated Compiler Kit

SACK is a toolkit for automatically generating compilers from instruction set definitions.

Given a instruction set description file - like you can find in `x86-platform.rkt` - it can compile code in its input pseudolanguage (an amalgamation of LISP and C) into working assembly language code for the target architecture.

This project is not usable in practice. It is a fine demonstration of the idea, but it would need a bunch of work:

 * The semantic tree datastructures need heavy refactoring for both speed and bugs.
 * The input system needs to be adapted to accept standard input languages, such as C.
 * The output system needs to be able to implement an assembler and generate the required headers to run directly on a system.
 * The semantic description language needs to be cleaned up, made less inconsistent, and needs to support graph-based rules, not just tree-based rules.
 * A stage needs to be added (and the description language extended) to support basic optimization strategies.

Very few of these require major innovations - just a bunch of time to implement.

## History

This project was created for a Computer Science Research class. It won 1st place in CS at a regional science fair, and a Yale Science and Engineering Assocation award, but did not advance to the state fair due to scheduling conflicts.

It is currently on hiatus, waiting for the author to have enough time to spend on it.

## Licensing

As this is not currently complete or usable, no license is provided. Please file a GitHub issue if you would like me to decide on a license so that you can use it.
