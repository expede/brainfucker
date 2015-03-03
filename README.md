# brainfucker
A simple [Brainfuck](http://esolangs.org/wiki/Brainfuck) interpreter in Haskell

| Branch | Build Status |
|--------|--------------|
| Master | [![Circle CI](https://circleci.com/gh/expede/brainfucker/tree/master.svg?style=svg)](https://circleci.com/gh/expede/brainfucker/tree/master) |

# Installation

```bash
cabal sandbox init
cabal install --only-dependencies --enable-tests
```

# Tests
```bash
cabal tests
```