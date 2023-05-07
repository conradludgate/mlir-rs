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

The LLVM project started working on something called [MLIR](https://mlir.llvm.org/). As far as I could tell, it was an effort driven mostly by Google.
It's a C++ framework for implemention multi level optimsing compilers. They show off some usecases like tensorflow computation graphs being modelled as MLIR and lowering down to CUDA.

I don't particularly want to use C++. Unfortunately, it relies on cmake and C++ abstract classes.
This project is an attempt to make a Rust rewrite. The current design is taken from the LLVM project, so see there for high level documentation for now.

## Concepts

This is an extremely bare and high level overview of the concepts. Expect it to change

### Dialects

"Levels" of IR are represented as a dialect. Each dialect is in charge of defining types and operations it can support.
The neat thing about MLIR is that is supports multiple dialects existing at once.

### Operations

Operations are the core of MLIR. This can represent high level concepts like "make an RPC call", mid level concepts like for loops and other built in functions, and low level concepts like syscalls, addition, jump-to-subroutine. These are defined by dialects

Operations have arguments and return values, and everything is typed. This doesn't necessarily mean strongly typed as your dialects can provide an "any" type.
Operations can also have metadata and source code information.

Operations can also be "terminators", this means they will continue into another block (eg a switch statement)

Lastly, Operations can contain regions. This can be used to represent functions. Eg defining a function is an operation, and the statements inside are blocks inside the contained region.

### Regions

Regions contain potentially multiple blocks.

### Blocks

A block is a sequence of operations. A block also has a name and a set of arguments.

In Rust terms, the name of a block could be thought of as a label, and the arguments can be interpreted as a set of captures.

### Passes

The end goal of MLIR is to take your high level IR and lower it into either a codegen IR or some other IR that can support an interpreter. To do this, we use the concept of a "pass".

Passes cover 2 core concepts:
1. Optimisations
2. Lowering

Both of these follow a similar system of recursively walking the tree of operations, finding patterns, and then converting to some other operation (or set of operations).

Passes can be defined on generic operations, or specific ones, or specific patterns of operations.


The way I'm currently thinking about this is that all operations have a weight to them. All passes should guarantee that the weight of the tree is reduced. I'm not sure if this is worth implementing however.

## Dialects

I plan to implement some of the basic dialects from MLIR (Affine, ControlFlow, Bufferization, Memref etc) as well as use CLIR instead of LLVMIR.

I am going with Cranelift instead of LLVM for 2 reasons:

1. It's written in Rust.
2. It's a much simpler codegen backend, performing little optimisation passes. Since MLIR is intended to be an optimisation framework already, it means we can take that burden instead.
