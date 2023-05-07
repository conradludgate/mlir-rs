# MLIR-RS

Multi-Level Intermediate Representation framework for Rust.

## What

Modern programming language design is moving towards multi-level lowering to get from AST to ASM. What does that mean?

For eaxmple, Rust goes through many stages of compilation levels. These include

* AST - Abstract Syntax Tree - an in memory representation of the rust source code
* HIR - High-level IR - This is the AST with some high level concepts removed like `for` and `while` loops and `if let` is turned into `match`.
* THIR - Typed HIR - HIR with all types checked and inferred, and all concepts of structs and traits are removed. All implicit behaviour like autoref are also made explicit here.
* MIR - Mid-level IR - This is THIR with all high level control flow structures removed. They are replaced entirely with jumps
* LIR - Low-level IR - This doesn't yet exist, but it's a theoretical lowering from MIR into a data-dependency graph.
* LLVMIR or CLIR - LLVM and CraneLift are backend compilers that eventually lower themselves into object files. Combined with a linker, these make an executible. They use their own IR.

That's a lot to keep track of, and as a result there is 560000 lines of code in the Rust compiler.

Other languages perform similar abstractions. It's a shame that all of this needs to be rewritten.

## Prior Art

The LLVM project started working on something called [MLIR](https://mlir.llvm.org/). As far as I could tell, it was an effort drove mostly by Google.
It's a C++ framework for implemention multi level optimsing compilers. They show off some usecases like tensorflow computation graphs being modelled as MLIR and lowering down to CUDA.

I don't particularly want to use C++. So this project is an attempt to make a Rust rewrite. The current design is taken from the LLVM project, so see there for high level documentation for now.

## Concepts

### Operations

### Regions

### Blocks

## Dialects

I plan to implement some of the basic dialects from MLIR (Affine, ControlFlow, Bufferization, Memref etc) as well as use CLIR instead of LLVMIR.

I am going with Cranelift instead of LLVM for 2 reasons:

1. It's written in Rust.
2. It's a much simpler codegen backend, performing little optimisation passes. Since MLIR is intended to be an optimisation framework already, it means we can take that burden instead.
