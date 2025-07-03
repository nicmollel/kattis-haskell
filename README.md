# Kattis Haskell Solutions

This repository contains Haskell solutions to competitive programming problems from [Kattis](https://open.kattis.com/).

## Solved Problems

| Problem             | Solution File                                | Kattis Link                                                                                           |
| ------------------- | -------------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| A Different Problem | [Different.hs](solutions/Different.hs)       | https://open.kattis.com/problems/different                                                            |
| Army Strength       | [ArmyStrength.hs](solutions/ArmyStrength.hs) | https://open.kattis.com/problems/armystrengthhard / https://open.kattis.com/problems/armystrengtheasy |
| Pot                 | [Pot.hs](solutions/Pot.hs)                   | https://open.kattis.com/problems/pot                                                                  |
| Popular Vote        | [Vote.hs](solutions/Vote.hs)                 | https://open.kattis.com/problems/vote                                                                 |

## Building and Running

**Build all solutions:**

```bash
cabal build
```

**Run a specific solution:**

```bash
cabal run different    # or pot, vote
```

**Test all solutions:**

```bash
cabal test
```

## Dependencies

This project uses minimal external dependencies:

- **mtl** (^>=2.3.1) - Monad transformer library (used by Scanner utility)

## Prerequisites

Install GHC and Cabal using [GHCup](https://www.haskell.org/ghcup/):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.10.1
ghcup install cabal recommended
```

## Solution Structure

Each solution follows a functional programming pattern using:

- Function composition with the `>>>` operator
- Point-free style programming
- The `interact` function for I/O processing

Solutions are organized as separate executables in the `solutions/` directory, with shared utilities in `src/`.

## Kattis Submission Process

Since Kattis only accepts single-file submissions without imports, each solution is tested in `submission/Main.hs` before uploading to Kattis. The submission file contains the complete solution code without any module imports or dependencies. After testing, this file is overwritten and reused for the next solution submission.

## AI Development Assistant

This project includes a `CLAUDE.md` file that provides guidance to Claude Code (claude.ai/code) when working with the codebase. This file contains:

- Project overview and architecture details
- Build commands and development workflow
- Coding patterns and conventions used in solutions
- Instructions for maintaining the functional programming style

The `CLAUDE.md` file helps ensure consistent development practices when using AI assistance for solving new Kattis problems or maintaining existing solutions.

### Usage

This project is all about learning, especially writing many small haskell programs to be familiar with the language. To that end, I have made some specific choices in AI usage.

1. Github Copilot is disabled on haskell files
2. I hand code my solution and validate it against input, then I will prompt claude on improvements to the solutions. During the solutioning, if I can't resolve compile errors my normal search, I will bring in AI for assistance.
3. I review the suggested improvements and adopt them as necessary.

This process for me strikes a balance of learning concepts and then building directly on that knowledge to see how it could be improved. Ultimately, I hope it will improve my mastery of the language.

## References

This section lists articles and repositories that were studied while working on these solutions:

- [Competitive Programming in Haskell: Scanner](https://byorgey.github.io/blog/posts/2019/05/22/competitive-programming-in-haskell-scanner.html) - Blog post about scanner utilities for competitive programming
- [comprog-hs Scanner.hs](https://github.com/byorgey/comprog-hs/blob/master/Scanner.hs) - Scanner utilities were copied from this repository
- [Competitive Programming in Haskell: reading large inputs with ByteString](https://byorgey.github.io/blog/posts/2019/10/12/competitive-programming-in-haskell-reading-large-inputs-with-bytestring.html) - When to abandon using `String` for efficiency
- [Compititive Programming Book: Methods to solve](https://cpbook.net/methodstosolve) - hints on underlying theory to solve problems
