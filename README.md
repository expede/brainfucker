# brainfucker
A simple [brainfuck](http://esolangs.org/wiki/Brainfuck) interpreter in Haskell

**NB: currently horribly broken. Doing a free monad redesign on another branch. Should be done in the near future ;)**

| Branch | Build Status - CircleCI | Build Status - Travis CI |
|--------|-------------------------|--------------------------|
| Master | [![Circle CI](https://circleci.com/gh/expede/brainfucker/tree/master.svg?style=svg)](https://circleci.com/gh/expede/brainfucker/tree/master) | ![](https://travis-ci.org/expede/brainfucker.svg) |

# Installation

```bash
cabal sandbox init
cabal install --only-dependencies --enable-tests
```

# Tests
## Tasty
> [Tasty](http://documentup.com/feuerbach/tasty) is a modern testing framework for Haskell.
> It lets you combine your unit tests, golden tests, QuickCheck/SmallCheck properties, and any other types of tests into a single test suite.

```bash
cabal test
```
