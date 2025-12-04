# Well Typed Qubits

A Haskell-embedded DSL for simulating and composing quantum computations, featuring type-safety in qubit manipulation through type-level programming.

## Features
- Static guarantee of the no-cloning theorem
- Gate abstractions for common quantum gates
- Monadic composition of quantum operations
- Explicit qubit manipulation
- Measurement, sampling, and simulation

## Executing

```
git clone git@github.com:Fleivio/Well-Typed-Qubits.git
cd Well-Typed-Qubits/
cabal run
```

## Project Tree

```
.
├── app
│   ├── Main.hs                  # Executable and Examples
│   ├── Deutsch.hs               # Deutsch-Jesza Algorithm
│   ├── Grover.hs                # Grover Algorithm
│   └── Teleport.hs              # Teleport Algorithm 
│  
│  
├── src
│   └── Quant
│       ├── Core
│       │   ├── Basis.hs         # Spacial Basis
│       │   ├── Bit.hs           # Classical Bit
│       │   ├── OP.hs            # Quantum Operations
│       │   ├── PA.hs            # Probability Amplitude
│       │   ├── QR.hs            # Quantum References
│       │   ├── QV.hs            # Quantum Values
│       │   └── Virt.hs          # Virtual Quantum Values
│       ├── List
│       │   ├── SList.hs         # Singleton Lists
│       │   └── Vec.hs           # Fixed-Length Vectors
│       ├── NoMonad.hs           # Examples without monadic operations
│       ├── QAct
│       │   ├── QAct.hs          # Quantum Action Monad
│       │   └── QBitAct.hs       # Specific Quantum Gates Construction for Qubits
│       ├── Quant.hs             # Main library exports
│       └── Quoters
│           ├── BitQuoter.hs     # Quotations for Vec
│           ├── MatrixQuoter.hs  # Quotations for Matrix
│           ├── NatLabel.hs      # Overloaded Labels for Naturals
│           └── SListQuoter.hs   # Quotations for SList
```