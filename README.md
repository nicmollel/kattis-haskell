# Kattis Haskell Solutions

This repository contains Haskell solutions to competitive programming problems from [Kattis](https://open.kattis.com/).

## Solved Problems

| Problem | Solution File | Kattis Link |
|---------|---------------|-------------|
| Different | [Different.hs](solutions/Different.hs) | https://open.kattis.com/problems/different |
| Pot | [Pot.hs](solutions/Pot.hs) | https://open.kattis.com/problems/pot |
| Vote | [Vote.hs](solutions/Vote.hs) | https://open.kattis.com/problems/vote |

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
ghcup install ghc 9.8.2
ghcup install cabal 3.10.2.0
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