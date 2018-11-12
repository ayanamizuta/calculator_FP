# calculator_FP

## Description

Let s_0,s_1,s_2... be standard **Free Independent** semicircular random variables, then the expectation of a non-commutative polynmial of s_0,s_1,s_2... is determined uniquely by combinatrial calculation.

This code enable us to parse a non-commutative polynomial of s_0,s_1,s_2... and calculate its expectation

## Dependencies

GHC 8.0.2
cabal-install version 1.22.9.0

It might be convenient to install `rlwrap` for easy-input

```bash
rlwrap -cr ghci
```

## DEMO

```bash
ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Prelude> :l semipolyexp.hs
[1 of 1] Compiling Main             ( semipolyexp.hs, interpreted )
Ok, modules loaded: Main.
*Main> main
s_0					// Standard semi-circular 
AST          :Right (Semicircular 0)
Expectation  :0
4-th cumulant:0
*Main> main
s_0^2					// Free Poission
AST          :Right (Expo (Semicircular 0) (Val 2))
Expectation  :1
4-th cumulant:1
*Main> main
s_0^3
AST          :Right (Expo (Semicircular 0) (Val 3))
Expectation  :0
4-th cumulant:82
*Main> main
s_0+s_1+s_2+s_3				// Sum of independent semi-circular
AST          :Right (Add (Add (Add (Semicircular 0) (Semicircular 1)) (Semicircular 2)) (Semicircular 3))
Expectation  :0
4-th cumulant:0
*Main> main
s_0^3+s_1^2-s_0*s_1+2*s_0*s_2*s_1
AST          :Right (Add (Sub (Add (Expo (Semicircular 0) (Val 3)) (Expo (Semicircular 1) (Val 2))) (Mul (Semicircular 0) (Semicircular 1))) (Mul (Mul (Mul (Val 2) (Semicircular 0)) (Semicircular 2)) (Semicircular 1)))
Expectation  :1
4-th cumulant:91
```