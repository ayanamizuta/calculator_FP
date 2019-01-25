# calculator_FP

## Description

Let s_0,s_1,s_2... be standard **Free Independent** semicircular random variables, then the expectation of a non-commutative polynmial of s_0,s_1,s_2... is determined uniquely by combinatrial calculation.

This code enable us to parse a non-commutative polynomial of s_0,s_1,s_2... and calculate its M-th moment in polynomial time with respect to M

An explanation of the implementation is given in https://arxiv.org/abs/1901.08210

## Dependencies

GHC 8.0.2
cabal-install version 1.22.9.0

## DEMO

```bash
ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Prelude> :l semipolyexp.hs
[1 of 1] Compiling Main             ( semipolyexp.hs, interpreted )

semipolyexp.hs:148:10: warning: [-Wmissing-methods]
    • No explicit implementation for
        ‘abs’, ‘signum’, ‘fromInteger’, and (either ‘negate’ or ‘-’)
    • In the instance declaration for ‘Num OneVarPolynomial’
    |
148 | instance Num OneVarPolynomial where
    |          ^^^^^^^^^^^^^^^^^^^^
Ok, one module loaded.
*Main> main
s_0						//a standard semicircular element
100
AST          :Sc 0
1978261657756160653623774456
*Main> main
s_0+s_1						//the sumation of two free standard semicircular elements
100
AST          :Add (Sc 0) (Sc 1)
2227324616177996201941033333974680463212544
*Main> 1978261657756160653623774456*2^50
2227324616177996201941033333974680463212544	//since s_0+s_1 is a semicircular element whose second moment is 2
*Main> main
s_0*s_1*s_2+s_2*s_1*s_0
100
AST          :Add (Mul (Mul (Sc 0) (Sc 1)) (Sc 2)) (Mul (Mul (Sc 2) (Sc 1)) (Sc 0))
1304162608063151910865972215441278247053341647095893584   // about 150 seconds
*Main> main
s_0*s_1*s_2+s_2*s_1*s_0
101
AST          :Add (Mul (Mul (Sc 0) (Sc 1)) (Sc 2)) (Mul (Mul (Sc 2) (Sc 1)) (Sc 0))
0