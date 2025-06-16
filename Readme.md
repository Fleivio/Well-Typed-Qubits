# Well Typed Qubits

A Haskell-embedded DSL for simulation and composing quantum computations, featuring type-safety over qubit manipulation through type-level programming.

## Features

- Static garantee of no-cloning theorem
- Gate abstractions for common quantum gates
- Monadic composition of quantum operations
- Explicit qubit manipulation
- Measurement, sampling and simulation

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
│   └── Main.hs                 # Executable and Examples
└── src
    └── Quant
        ├── Core
        │   ├── Basis.hs        # Spacial Basis
        │   ├── Bit.hs          # Classical Bit
        │   ├── OP.hs           # Quantum Operations
        │   ├── PA.hs           # Probability Amplitude
        │   ├── QR.hs           # Quantum References
        │   ├── QV.hs           # Quantum Values
        │   └── Virt.hs         # Virtual Quantum Values
        ├── List
        │   ├── OvLabel.hs      # Overloaded Labels for Naturals
        │   ├── SList.hs        # Singleton Lists
        │   └── Vec.hs          # Fixed-Length Vectors
        ├── QAct
        │   ├── QAct.hs         # Quantum Action Monad
        │   └── QBitAct.hs      # Specific Quantum Gates Contruction for Qubits
        ├── NoMonad.hs          # Examples without monadic operations
        ├── BitQuoter.hs        # Quotations for memory initialization
        └── Quant.hs            # Main library exports
```

## Further Work

- [ ] Implement interface to n-qubit circuits such as deutsch-jozsa, grover, shor, etc.
- [ ] Implement writer monad to allow history tracking of operations.
- [ ] Quotes for Matrix.