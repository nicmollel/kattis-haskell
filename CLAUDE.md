# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell project for solving Kattis competitive programming problems. The repository contains:

- Individual solution files in the `solutions/` directory, each as standalone executable programs
- Each solution corresponds to a specific Kattis problem and includes the problem URL as a comment
- Solutions use functional programming patterns with point-free style and the `>>>` composition operator

## Build Commands

**Build all executables:**
```bash
cabal build
```

**Build specific solution:**
```bash
cabal build different  # or any other executable name from .cabal file
```

**Run specific solution:**
```bash
cabal run different    # runs with stdin input
cabal run pot
```

**Run tests:**
```bash
cabal test
```

**Clean build artifacts:**
```bash
cabal clean
```

## Architecture

The project uses Cabal for build management with these key components:

- **solutions/**: Each `.hs` file is a complete Kattis solution with its own `main` function
- **src/**: Contains shared library code (currently minimal - just `Scanner.hs`)
- **test/**: Test suite (currently not implemented)

Each solution in `solutions/` is configured as a separate executable in the `.cabal` file with:
- Custom main module specified via `ghc-options: -main-is ModuleName`
- Individual `main-is` pointing to the solution file
- Base dependencies only

## Solution Pattern

Kattis solutions follow this functional pattern:
```haskell
main :: IO ()
main = interact $
  lines >>> map processLine >>> unlines
  -- or similar composition using >>> operator
```

Solutions use:
- `Control.Category ((>>>))` for function composition
- `interact` for stdin/stdout processing
- Point-free programming style
- Problem URL as comment at top of file
