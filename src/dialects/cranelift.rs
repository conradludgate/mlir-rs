#![allow(dead_code, unused_mut, unused_variables, non_snake_case)]
mod ir {
    pub struct Block;
    pub struct Type;
    pub struct Value;
    pub struct JumpTable;
    pub struct TrapCode;
    pub struct FuncRef;
    pub struct SigRef;
    pub struct MemFlags;
    pub struct StackSlot;
    pub struct DynamicStackSlot;
    pub struct GlobalValue;
    pub struct Table;
    pub struct Constant;
    pub struct Immediate;
    pub mod immediates {
        pub struct Uimm8;
        pub struct Uimm16;
        pub struct Uimm32;
        pub struct Uimm64;
        pub struct Imm8;
        pub struct Imm16;
        pub struct Imm32;
        pub struct Imm64;
        pub struct Ieee32;
        pub struct Ieee64;
        pub struct Offset32;
    }
    pub mod condcodes {
        pub struct IntCC;
        pub struct FloatCC;
    }
    pub struct AtomicRmwOp;
}
struct Value;
struct Inst;

/// Jump.
///
/// Unconditionally jump to a basic block, passing the specified
/// block arguments. The number and types of arguments must match the
/// destination block.
///
/// Inputs:
///
/// - block_call_label: Destination basic block
/// - block_call_args: Block arguments
fn jump(mut block_call_label: ir::Block, block_call_args: &[Value]) -> Inst {
    todo!()
}

/// Conditional branch when cond is non-zero.
///
/// Take the ``then`` branch when ``c != 0``, and the ``else`` branch otherwise.
///
/// Inputs:
///
/// - c: Controlling value to test
/// - block_then_label: Destination basic block
/// - block_then_args: Block arguments
/// - block_else_label: Destination basic block
/// - block_else_args: Block arguments
fn brif(
    c: ir::Value,
    block_then_label: ir::Block,
    block_then_args: &[Value],
    block_else_label: ir::Block,
    block_else_args: &[Value],
) -> Inst {
    todo!()
}

/// Indirect branch via jump table.
///
/// Use ``x`` as an unsigned index into the jump table ``JT``. If a jump
/// table entry is found, branch to the corresponding block. If no entry was
/// found or the index is out-of-bounds, branch to the default block of the
/// table.
///
/// Note that this branch instruction can't pass arguments to the targeted
/// blocks. Split critical edges as needed to work around this.
///
/// Do not confuse this with "tables" in WebAssembly. ``br_table`` is for
/// jump tables with destinations within the current function only -- think
/// of a ``match`` in Rust or a ``switch`` in C.  If you want to call a
/// function in a dynamic library, that will typically use
/// ``call_indirect``.
///
/// Inputs:
///
/// - x: i32 index into jump table
/// - JT: A jump table.
fn br_table(x: ir::Value, JT: ir::JumpTable) -> Inst {
    todo!()
}

/// Encodes an assembly debug trap.
fn debugtrap() -> Inst {
    todo!()
}

/// Terminate execution unconditionally.
///
/// Inputs:
///
/// - code: A trap reason code.
fn trap<T1: Into<ir::TrapCode>>(code: T1) -> Inst {
    todo!()
}

/// Trap when zero.
///
/// if ``c`` is non-zero, execution continues at the following instruction.
///
/// Inputs:
///
/// - c: Controlling value to test
/// - code: A trap reason code.
fn trapz<T1: Into<ir::TrapCode>>(c: ir::Value, code: T1) -> Inst {
    todo!()
}

/// A resumable trap.
///
/// This instruction allows non-conditional traps to be used as non-terminal instructions.
///
/// Inputs:
///
/// - code: A trap reason code.
fn resumable_trap<T1: Into<ir::TrapCode>>(code: T1) -> Inst {
    todo!()
}

/// Trap when non-zero.
///
/// If ``c`` is zero, execution continues at the following instruction.
///
/// Inputs:
///
/// - c: Controlling value to test
/// - code: A trap reason code.
fn trapnz<T1: Into<ir::TrapCode>>(c: ir::Value, code: T1) -> Inst {
    todo!()
}

/// A resumable trap to be called when the passed condition is non-zero.
///
/// If ``c`` is zero, execution continues at the following instruction.
///
/// Inputs:
///
/// - c: Controlling value to test
/// - code: A trap reason code.
fn resumable_trapnz<T1: Into<ir::TrapCode>>(c: ir::Value, code: T1) -> Inst {
    todo!()
}

/// Return from the function.
///
/// Unconditionally transfer control to the calling function, passing the
/// provided return values. The list of return values must match the
/// function signature's return types.
///
/// Inputs:
///
/// - rvals: return values
fn return_(mut rvals: &[Value]) -> Inst {
    todo!()
}

/// Direct function call.
///
/// Call a function which has been declared in the preamble. The argument
/// types must match the function's signature.
///
/// Inputs:
///
/// - FN: function to call, declared by `function`
/// - args: call arguments
///
/// Outputs:
///
/// - rvals: return values
fn call(mut FN: ir::FuncRef, args: &[Value]) -> Inst {
    todo!()
}

/// Indirect function call.
///
/// Call the function pointed to by `callee` with the given arguments. The
/// called function must match the specified signature.
///
/// Note that this is different from WebAssembly's ``call_indirect``; the
/// callee is a native address, rather than a table index. For WebAssembly,
/// `table_addr` and `load` are used to obtain a native address
/// from a table.
///
/// Inputs:
///
/// - SIG: function signature
/// - callee: address of function to call
/// - args: call arguments
///
/// Outputs:
///
/// - rvals: return values
fn call_indirect(mut SIG: ir::SigRef, callee: ir::Value, args: &[Value]) -> Inst {
    todo!()
}

/// Direct tail call.
///
/// Tail call a function which has been declared in the preamble. The
/// argument types must match the function's signature, the caller and
/// callee calling conventions must be the same, and must be a calling
/// convention that supports tail calls.
///
/// This instruction is a block terminator.
///
/// Inputs:
///
/// - FN: function to call, declared by `function`
/// - args: call arguments
fn return_call(mut FN: ir::FuncRef, args: &[Value]) -> Inst {
    todo!()
}

/// Indirect tail call.
///
/// Call the function pointed to by `callee` with the given arguments. The
/// argument types must match the function's signature, the caller and
/// callee calling conventions must be the same, and must be a calling
/// convention that supports tail calls.
///
/// This instruction is a block terminator.
///
/// Note that this is different from WebAssembly's ``tail_call_indirect``;
/// the callee is a native address, rather than a table index. For
/// WebAssembly, `table_addr` and `load` are used to obtain a native address
/// from a table.
///
/// Inputs:
///
/// - SIG: function signature
/// - callee: address of function to call
/// - args: call arguments
fn return_call_indirect(mut SIG: ir::SigRef, callee: ir::Value, args: &[Value]) -> Inst {
    todo!()
}

/// Get the address of a function.
///
/// Compute the absolute address of a function declared in the preamble.
/// The returned address can be used as a ``callee`` argument to
/// `call_indirect`. This is also a method for calling functions that
/// are too far away to be addressable by a direct `call`
/// instruction.
///
/// Inputs:
///
/// - iAddr (controlling type variable): An integer address type
/// - FN: function to call, declared by `function`
///
/// Outputs:
///
/// - addr: An integer address type
fn func_addr(iAddr: ir::Type, FN: ir::FuncRef) -> Value {
    todo!()
}

/// Vector splat.
///
/// Return a vector whose lanes are all ``x``.
///
/// Inputs:
///
/// - TxN (controlling type variable): A SIMD vector type
/// - x: Value to splat to all lanes
///
/// Outputs:
///
/// - a: A SIMD vector type
fn splat(TxN: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Vector swizzle.
///
/// Returns a new vector with byte-width lanes selected from the lanes of the first input
/// vector ``x`` specified in the second input vector ``s``. The indices ``i`` in range
/// ``[0, 15]`` select the ``i``-th element of ``x``. For indices outside of the range the
/// resulting lane is 0. Note that this operates on byte-width lanes.
///
/// Inputs:
///
/// - x: Vector to modify by re-arranging lanes
/// - y: Mask for re-arranging lanes
///
/// Outputs:
///
/// - a: A SIMD vector type consisting of 16 lanes of 8-bit integers
fn swizzle(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// A vector swizzle lookalike which has the semantics of `pshufb` on x64.
///
/// This instruction will permute the 8-bit lanes of `x` with the indices
/// specified in `y`. Each lane in the mask, `y`, uses the bottom four
/// bits for selecting the lane from `x` unless the most significant bit
/// is set, in which case the lane is zeroed. The output vector will have
/// the following contents when the element of `y` is in these ranges:
///
/// * `[0, 127]` -> `x[y[i] % 16]`
/// * `[128, 255]` -> 0
///
/// Inputs:
///
/// - x: Vector to modify by re-arranging lanes
/// - y: Mask for re-arranging lanes
///
/// Outputs:
///
/// - a: A SIMD vector type consisting of 16 lanes of 8-bit integers
fn x86_pshufb(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Insert ``y`` as lane ``Idx`` in x.
///
/// The lane index, ``Idx``, is an immediate value, not an SSA value. It
/// must indicate a valid lane index for the type of ``x``.
///
/// Inputs:
///
/// - x: The vector to modify
/// - y: New lane value
/// - Idx: Lane index
///
/// Outputs:
///
/// - a: A SIMD vector type
fn insertlane<T1: Into<ir::immediates::Uimm8>>(x: ir::Value, y: ir::Value, Idx: T1) -> Value {
    todo!()
}

/// Extract lane ``Idx`` from ``x``.
///
/// The lane index, ``Idx``, is an immediate value, not an SSA value. It
/// must indicate a valid lane index for the type of ``x``. Note that the upper bits of ``a``
/// may or may not be zeroed depending on the ISA but the type system should prevent using
/// ``a`` as anything other than the extracted value.
///
/// Inputs:
///
/// - x: A SIMD vector type
/// - Idx: Lane index
///
/// Outputs:
///
/// - a:
fn extractlane<T1: Into<ir::immediates::Uimm8>>(x: ir::Value, Idx: T1) -> Value {
    todo!()
}

/// Signed integer minimum.
///
/// Inputs:
///
/// - x: A scalar or vector integer type
/// - y: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn smin(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Unsigned integer minimum.
///
/// Inputs:
///
/// - x: A scalar or vector integer type
/// - y: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn umin(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Signed integer maximum.
///
/// Inputs:
///
/// - x: A scalar or vector integer type
/// - y: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn smax(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Unsigned integer maximum.
///
/// Inputs:
///
/// - x: A scalar or vector integer type
/// - y: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn umax(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Unsigned average with rounding: `a := (x + y + 1) // 2`
///
/// The addition does not lose any information (such as from overflow).
///
/// Inputs:
///
/// - x: A SIMD vector type containing integers
/// - y: A SIMD vector type containing integers
///
/// Outputs:
///
/// - a: A SIMD vector type containing integers
fn avg_round(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Add with unsigned saturation.
///
/// This is similar to `iadd` but the operands are interpreted as unsigned integers and their
/// summed result, instead of wrapping, will be saturated to the highest unsigned integer for
/// the controlling type (e.g. `0xFF` for i8).
///
/// Inputs:
///
/// - x: A SIMD vector type containing integers
/// - y: A SIMD vector type containing integers
///
/// Outputs:
///
/// - a: A SIMD vector type containing integers
fn uadd_sat(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Add with signed saturation.
///
/// This is similar to `iadd` but the operands are interpreted as signed integers and their
/// summed result, instead of wrapping, will be saturated to the lowest or highest
/// signed integer for the controlling type (e.g. `0x80` or `0x7F` for i8). For example,
/// since an `sadd_sat.i8` of `0x70` and `0x70` is greater than `0x7F`, the result will be
/// clamped to `0x7F`.
///
/// Inputs:
///
/// - x: A SIMD vector type containing integers
/// - y: A SIMD vector type containing integers
///
/// Outputs:
///
/// - a: A SIMD vector type containing integers
fn sadd_sat(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Subtract with unsigned saturation.
///
/// This is similar to `isub` but the operands are interpreted as unsigned integers and their
/// difference, instead of wrapping, will be saturated to the lowest unsigned integer for
/// the controlling type (e.g. `0x00` for i8).
///
/// Inputs:
///
/// - x: A SIMD vector type containing integers
/// - y: A SIMD vector type containing integers
///
/// Outputs:
///
/// - a: A SIMD vector type containing integers
fn usub_sat(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Subtract with signed saturation.
///
/// This is similar to `isub` but the operands are interpreted as signed integers and their
/// difference, instead of wrapping, will be saturated to the lowest or highest
/// signed integer for the controlling type (e.g. `0x80` or `0x7F` for i8).
///
/// Inputs:
///
/// - x: A SIMD vector type containing integers
/// - y: A SIMD vector type containing integers
///
/// Outputs:
///
/// - a: A SIMD vector type containing integers
fn ssub_sat(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Load from memory at ``p + Offset``.
///
/// This is a polymorphic instruction that can load any value type which
/// has a memory representation.
///
/// Inputs:
///
/// - Mem (controlling type variable): Any type that can be stored in memory
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: Value loaded
fn load<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    Mem: ir::Type,
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Store ``x`` to memory at ``p + Offset``.
///
/// This is a polymorphic instruction that can store any value type with a
/// memory representation.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - x: Value to be stored
/// - p: An integer address type
/// - Offset: Byte offset from base address
fn store<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    MemFlags: T1,
    x: ir::Value,
    p: ir::Value,
    Offset: T2,
) -> Inst {
    todo!()
}

/// Load 8 bits from memory at ``p + Offset`` and zero-extend.
///
/// This is equivalent to ``load.i8`` followed by ``uextend``.
///
/// Inputs:
///
/// - iExt8 (controlling type variable): An integer type with more than 8 bits
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: An integer type with more than 8 bits
fn uload8<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    iExt8: ir::Type,
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Load 8 bits from memory at ``p + Offset`` and sign-extend.
///
/// This is equivalent to ``load.i8`` followed by ``sextend``.
///
/// Inputs:
///
/// - iExt8 (controlling type variable): An integer type with more than 8 bits
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: An integer type with more than 8 bits
fn sload8<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    iExt8: ir::Type,
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Store the low 8 bits of ``x`` to memory at ``p + Offset``.
///
/// This is equivalent to ``ireduce.i8`` followed by ``store.i8``.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - x: An integer type with more than 8 bits
/// - p: An integer address type
/// - Offset: Byte offset from base address
fn istore8<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    MemFlags: T1,
    x: ir::Value,
    p: ir::Value,
    Offset: T2,
) -> Inst {
    todo!()
}

/// Load 16 bits from memory at ``p + Offset`` and zero-extend.
///
/// This is equivalent to ``load.i16`` followed by ``uextend``.
///
/// Inputs:
///
/// - iExt16 (controlling type variable): An integer type with more than 16 bits
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: An integer type with more than 16 bits
fn uload16<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    iExt16: ir::Type,
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Load 16 bits from memory at ``p + Offset`` and sign-extend.
///
/// This is equivalent to ``load.i16`` followed by ``sextend``.
///
/// Inputs:
///
/// - iExt16 (controlling type variable): An integer type with more than 16 bits
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: An integer type with more than 16 bits
fn sload16<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    iExt16: ir::Type,
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Store the low 16 bits of ``x`` to memory at ``p + Offset``.
///
/// This is equivalent to ``ireduce.i16`` followed by ``store.i16``.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - x: An integer type with more than 16 bits
/// - p: An integer address type
/// - Offset: Byte offset from base address
fn istore16<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    MemFlags: T1,
    x: ir::Value,
    p: ir::Value,
    Offset: T2,
) -> Inst {
    todo!()
}

/// Load 32 bits from memory at ``p + Offset`` and zero-extend.
///
/// This is equivalent to ``load.i32`` followed by ``uextend``.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: An integer type with more than 32 bits
fn uload32<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Load 32 bits from memory at ``p + Offset`` and sign-extend.
///
/// This is equivalent to ``load.i32`` followed by ``sextend``.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: An integer type with more than 32 bits
fn sload32<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Store the low 32 bits of ``x`` to memory at ``p + Offset``.
///
/// This is equivalent to ``ireduce.i32`` followed by ``store.i32``.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - x: An integer type with more than 32 bits
/// - p: An integer address type
/// - Offset: Byte offset from base address
fn istore32<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    MemFlags: T1,
    x: ir::Value,
    p: ir::Value,
    Offset: T2,
) -> Inst {
    todo!()
}

/// Load an 8x8 vector (64 bits) from memory at ``p + Offset`` and zero-extend into an i16x8
/// vector.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: Value loaded
fn uload8x8<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Load an 8x8 vector (64 bits) from memory at ``p + Offset`` and sign-extend into an i16x8
/// vector.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: Value loaded
fn sload8x8<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Load a 16x4 vector (64 bits) from memory at ``p + Offset`` and zero-extend into an i32x4
/// vector.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: Value loaded
fn uload16x4<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Load a 16x4 vector (64 bits) from memory at ``p + Offset`` and sign-extend into an i32x4
/// vector.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: Value loaded
fn sload16x4<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Load an 32x2 vector (64 bits) from memory at ``p + Offset`` and zero-extend into an i64x2
/// vector.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: Value loaded
fn uload32x2<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Load a 32x2 vector (64 bits) from memory at ``p + Offset`` and sign-extend into an i64x2
/// vector.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - Offset: Byte offset from base address
///
/// Outputs:
///
/// - a: Value loaded
fn sload32x2<T1: Into<ir::MemFlags>, T2: Into<ir::immediates::Offset32>>(
    MemFlags: T1,
    p: ir::Value,
    Offset: T2,
) -> Value {
    todo!()
}

/// Load a value from a stack slot at the constant offset.
///
/// This is a polymorphic instruction that can load any value type which
/// has a memory representation.
///
/// The offset is an immediate constant, not an SSA value. The memory
/// access cannot go out of bounds, i.e.
/// `sizeof(a) + Offset <= sizeof(SS)`.
///
/// Inputs:
///
/// - Mem (controlling type variable): Any type that can be stored in memory
/// - SS: A stack slot
/// - Offset: In-bounds offset into stack slot
///
/// Outputs:
///
/// - a: Value loaded
fn stack_load<T1: Into<ir::immediates::Offset32>>(
    Mem: ir::Type,
    SS: ir::StackSlot,
    Offset: T1,
) -> Value {
    todo!()
}

/// Store a value to a stack slot at a constant offset.
///
/// This is a polymorphic instruction that can store any value type with a
/// memory representation.
///
/// The offset is an immediate constant, not an SSA value. The memory
/// access cannot go out of bounds, i.e.
/// `sizeof(a) + Offset <= sizeof(SS)`.
///
/// Inputs:
///
/// - x: Value to be stored
/// - SS: A stack slot
/// - Offset: In-bounds offset into stack slot
fn stack_store<T1: Into<ir::immediates::Offset32>>(
    x: ir::Value,
    SS: ir::StackSlot,
    Offset: T1,
) -> Inst {
    todo!()
}

/// Get the address of a stack slot.
///
/// Compute the absolute address of a byte in a stack slot. The offset must
/// refer to a byte inside the stack slot:
/// `0 <= Offset < sizeof(SS)`.
///
/// Inputs:
///
/// - iAddr (controlling type variable): An integer address type
/// - SS: A stack slot
/// - Offset: In-bounds offset into stack slot
///
/// Outputs:
///
/// - addr: An integer address type
fn stack_addr<T1: Into<ir::immediates::Offset32>>(
    iAddr: ir::Type,
    SS: ir::StackSlot,
    Offset: T1,
) -> Value {
    todo!()
}

/// Load a value from a dynamic stack slot.
///
/// This is a polymorphic instruction that can load any value type which
/// has a memory representation.
///
/// Inputs:
///
/// - Mem (controlling type variable): Any type that can be stored in memory
/// - DSS: A dynamic stack slot
///
/// Outputs:
///
/// - a: Value loaded
fn dynamic_stack_load(Mem: ir::Type, DSS: ir::DynamicStackSlot) -> Value {
    todo!()
}

/// Store a value to a dynamic stack slot.
///
/// This is a polymorphic instruction that can store any dynamic value type with a
/// memory representation.
///
/// Inputs:
///
/// - x: Value to be stored
/// - DSS: A dynamic stack slot
fn dynamic_stack_store(x: ir::Value, DSS: ir::DynamicStackSlot) -> Inst {
    todo!()
}

/// Get the address of a dynamic stack slot.
///
/// Compute the absolute address of the first byte of a dynamic stack slot.
///
/// Inputs:
///
/// - iAddr (controlling type variable): An integer address type
/// - DSS: A dynamic stack slot
///
/// Outputs:
///
/// - addr: An integer address type
fn dynamic_stack_addr(iAddr: ir::Type, DSS: ir::DynamicStackSlot) -> Value {
    todo!()
}

/// Compute the value of global GV.
///
/// Inputs:
///
/// - Mem (controlling type variable): Any type that can be stored in memory
/// - GV: A global value.
///
/// Outputs:
///
/// - a: Value loaded
fn global_value(Mem: ir::Type, GV: ir::GlobalValue) -> Value {
    todo!()
}

/// Compute the value of global GV, which is a symbolic value.
///
/// Inputs:
///
/// - Mem (controlling type variable): Any type that can be stored in memory
/// - GV: A global value.
///
/// Outputs:
///
/// - a: Value loaded
fn symbol_value(Mem: ir::Type, GV: ir::GlobalValue) -> Value {
    todo!()
}

/// Compute the value of global GV, which is a TLS (thread local storage) value.
///
/// Inputs:
///
/// - Mem (controlling type variable): Any type that can be stored in memory
/// - GV: A global value.
///
/// Outputs:
///
/// - a: Value loaded
fn tls_value(Mem: ir::Type, GV: ir::GlobalValue) -> Value {
    todo!()
}

/// Gets the content of the pinned register, when it's enabled.
///
/// Inputs:
///
/// - iAddr (controlling type variable): An integer address type
///
/// Outputs:
///
/// - addr: An integer address type
fn get_pinned_reg(iAddr: ir::Type) -> Value {
    todo!()
}

/// Sets the content of the pinned register, when it's enabled.
///
/// Inputs:
///
/// - addr: An integer address type
fn set_pinned_reg(addr: ir::Value) -> Inst {
    todo!()
}

/// Get the address in the frame pointer register.
///
/// Usage of this instruction requires setting `preserve_frame_pointers` to `true`.
///
/// Inputs:
///
/// - iAddr (controlling type variable): An integer address type
///
/// Outputs:
///
/// - addr: An integer address type
fn get_frame_pointer(iAddr: ir::Type) -> Value {
    todo!()
}

/// Get the address in the stack pointer register.
///
/// Inputs:
///
/// - iAddr (controlling type variable): An integer address type
///
/// Outputs:
///
/// - addr: An integer address type
fn get_stack_pointer(iAddr: ir::Type) -> Value {
    todo!()
}

/// Get the PC where this function will transfer control to when it returns.
///
/// Usage of this instruction requires setting `preserve_frame_pointers` to `true`.
///
/// Inputs:
///
/// - iAddr (controlling type variable): An integer address type
///
/// Outputs:
///
/// - addr: An integer address type
fn get_return_address(iAddr: ir::Type) -> Value {
    todo!()
}

/// Bounds check and compute absolute address of a table entry.
///
/// Verify that the offset ``p`` is in bounds for the table T, and generate
/// an absolute address that is safe to dereference.
///
/// ``Offset`` must be less than the size of a table element.
///
/// 1. If ``p`` is not greater than the table bound, return an absolute
///    address corresponding to a byte offset of ``p`` from the table's
///    base address.
/// 2. If ``p`` is greater than the table bound, generate a trap.
///
/// Inputs:
///
/// - iAddr (controlling type variable): An integer address type
/// - T: A table.
/// - p: An unsigned table offset
/// - Offset: Byte offset from element address
///
/// Outputs:
///
/// - addr: An integer address type
fn table_addr<T1: Into<ir::immediates::Offset32>>(
    iAddr: ir::Type,
    T: ir::Table,
    p: ir::Value,
    Offset: T1,
) -> Value {
    todo!()
}

/// Integer constant.
///
/// Create a scalar integer SSA value with an immediate constant value, or
/// an integer vector where all the lanes have the same value.
///
/// Inputs:
///
/// - NarrowInt (controlling type variable): An integer type of width up to `i64`
/// - N: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A constant integer scalar or vector value
fn iconst<T1: Into<ir::immediates::Imm64>>(NarrowInt: ir::Type, N: T1) -> Value {
    todo!()
}

/// Floating point constant.
///
/// Create a `f32` SSA value with an immediate constant value.
///
/// Inputs:
///
/// - N: A 32-bit immediate floating point number.
///
/// Outputs:
///
/// - a: A constant f32 scalar value
fn f32const<T1: Into<ir::immediates::Ieee32>>(N: T1) -> Value {
    todo!()
}

/// Floating point constant.
///
/// Create a `f64` SSA value with an immediate constant value.
///
/// Inputs:
///
/// - N: A 64-bit immediate floating point number.
///
/// Outputs:
///
/// - a: A constant f64 scalar value
fn f64const<T1: Into<ir::immediates::Ieee64>>(N: T1) -> Value {
    todo!()
}

/// SIMD vector constant.
///
/// Construct a vector with the given immediate bytes.
///
/// Inputs:
///
/// - TxN (controlling type variable): A SIMD vector type
/// - N: The 16 immediate bytes of a 128-bit vector
///
/// Outputs:
///
/// - a: A constant vector value
fn vconst<T1: Into<ir::Constant>>(TxN: ir::Type, N: T1) -> Value {
    todo!()
}

/// SIMD vector shuffle.
///
/// Shuffle two vectors using the given immediate bytes. For each of the 16 bytes of the
/// immediate, a value i of 0-15 selects the i-th element of the first vector and a value i of
/// 16-31 selects the (i-16)th element of the second vector. Immediate values outside of the
/// 0-31 range are not valid.
///
/// Inputs:
///
/// - a: A vector value
/// - b: A vector value
/// - mask: The 16 immediate bytes used for selecting the elements to shuffle
///
/// Outputs:
///
/// - a: A vector value
fn shuffle<T1: Into<ir::Immediate>>(a: ir::Value, b: ir::Value, mask: T1) -> Value {
    todo!()
}

/// Null constant value for reference types.
///
/// Create a scalar reference SSA value with a constant null value.
///
/// Inputs:
///
/// - Ref (controlling type variable): A scalar reference type
///
/// Outputs:
///
/// - a: A constant reference null value
fn null(Ref: ir::Type) -> Value {
    todo!()
}

/// Just a dummy instruction.
///
/// Note: this doesn't compile to a machine code nop.
fn nop() -> Inst {
    todo!()
}

/// Conditional select.
///
/// This instruction selects whole values. Use `bitselect` to choose each
/// bit according to a mask.
///
/// Inputs:
///
/// - c: Controlling value to test
/// - x: Value to use when `c` is true
/// - y: Value to use when `c` is false
///
/// Outputs:
///
/// - a: Any integer, float, or reference scalar or vector type
fn select(c: ir::Value, x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Conditional select intended for Spectre guards.
///
/// This operation is semantically equivalent to a select instruction.
/// However, it is guaranteed to not be removed or otherwise altered by any
/// optimization pass, and is guaranteed to result in a conditional-move
/// instruction, not a branch-based lowering.  As such, it is suitable
/// for use when producing Spectre guards. For example, a bounds-check
/// may guard against unsafe speculation past a bounds-check conditional
/// branch by passing the address or index to be accessed through a
/// conditional move, also gated on the same condition. Because no
/// Spectre-vulnerable processors are known to perform speculation on
/// conditional move instructions, this is guaranteed to pick the
/// correct input. If the selected input in case of overflow is a "safe"
/// value, for example a null pointer that causes an exception in the
/// speculative path, this ensures that no Spectre vulnerability will
/// exist.
///
/// Inputs:
///
/// - c: Controlling value to test
/// - x: Value to use when `c` is true
/// - y: Value to use when `c` is false
///
/// Outputs:
///
/// - a: Any integer, float, or reference scalar or vector type
fn select_spectre_guard(c: ir::Value, x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Conditional select of bits.
///
/// For each bit in `c`, this instruction selects the corresponding bit from `x` if the bit
/// in `x` is 1 and the corresponding bit from `y` if the bit in `c` is 0. See also:
/// `select`.
///
/// Inputs:
///
/// - c: Controlling value to test
/// - x: Value to use when `c` is true
/// - y: Value to use when `c` is false
///
/// Outputs:
///
/// - a: Any integer, float, or reference scalar or vector type
fn bitselect(c: ir::Value, x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// A bitselect-lookalike instruction except with the semantics of
/// `blendv`-related instructions on x86.
///
/// This instruction will use the top bit of each lane in `c`, the condition
/// mask. If the bit is 1 then the corresponding lane from `x` is chosen.
/// Otherwise the corresponding lane from `y` is chosen.
///
/// Inputs:
///
/// - c: Controlling value to test
/// - x: Value to use when `c` is true
/// - y: Value to use when `c` is false
///
/// Outputs:
///
/// - a: Any integer, float, or reference scalar or vector type
fn x86_blendv(c: ir::Value, x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Reduce a vector to a scalar boolean.
///
/// Return a scalar boolean true if any lane in ``a`` is non-zero, false otherwise.
///
/// Inputs:
///
/// - a: A SIMD vector type
///
/// Outputs:
///
/// - s: An integer type with 8 bits.
/// WARNING: arithmetic on 8bit integers is incomplete
fn vany_true(a: ir::Value) -> Value {
    todo!()
}

/// Reduce a vector to a scalar boolean.
///
/// Return a scalar boolean true if all lanes in ``i`` are non-zero, false otherwise.
///
/// Inputs:
///
/// - a: A SIMD vector type
///
/// Outputs:
///
/// - s: An integer type with 8 bits.
/// WARNING: arithmetic on 8bit integers is incomplete
fn vall_true(a: ir::Value) -> Value {
    todo!()
}

/// Reduce a vector to a scalar integer.
///
/// Return a scalar integer, consisting of the concatenation of the most significant bit
/// of each lane of ``a``.
///
/// Inputs:
///
/// - Int (controlling type variable): A scalar or vector integer type
/// - a: A SIMD vector type
///
/// Outputs:
///
/// - x: A scalar or vector integer type
fn vhigh_bits(Int: ir::Type, a: ir::Value) -> Value {
    todo!()
}

/// Integer comparison.
///
/// The condition code determines if the operands are interpreted as signed
/// or unsigned integers.
///
/// | Signed | Unsigned | Condition             |
/// |--------|----------|-----------------------|
/// | eq     | eq       | Equal                 |
/// | ne     | ne       | Not equal             |
/// | slt    | ult      | Less than             |
/// | sge    | uge      | Greater than or equal |
/// | sgt    | ugt      | Greater than          |
/// | sle    | ule      | Less than or equal    |
///
/// When this instruction compares integer vectors, it returns a vector of
/// lane-wise comparisons.
///
/// Inputs:
///
/// - Cond: An integer comparison condition code.
/// - x: A scalar or vector integer type
/// - y: A scalar or vector integer type
///
/// Outputs:
///
/// - a:
fn icmp<T1: Into<ir::condcodes::IntCC>>(Cond: T1, x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Compare scalar integer to a constant.
///
/// This is the same as the `icmp` instruction, except one operand is
/// a sign extended 64 bit immediate constant.
///
/// This instruction can only compare scalars. Use `icmp` for
/// lane-wise vector comparisons.
///
/// Inputs:
///
/// - Cond: An integer comparison condition code.
/// - x: A scalar integer type
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: An integer type with 8 bits.
/// WARNING: arithmetic on 8bit integers is incomplete
fn icmp_imm<T1: Into<ir::condcodes::IntCC>, T2: Into<ir::immediates::Imm64>>(
    Cond: T1,
    x: ir::Value,
    Y: T2,
) -> Value {
    todo!()
}

/// Wrapping integer addition: `a := x + y \pmod{2^B}`.
///
/// This instruction does not depend on the signed/unsigned interpretation
/// of the operands.
///
/// Inputs:
///
/// - x: A scalar or vector integer type
/// - y: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn iadd(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Wrapping integer subtraction: `a := x - y \pmod{2^B}`.
///
/// This instruction does not depend on the signed/unsigned interpretation
/// of the operands.
///
/// Inputs:
///
/// - x: A scalar or vector integer type
/// - y: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn isub(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Integer negation: `a := -x \pmod{2^B}`.
///
/// Inputs:
///
/// - x: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn ineg(x: ir::Value) -> Value {
    todo!()
}

/// Integer absolute value with wrapping: `a := |x|`.
///
/// Inputs:
///
/// - x: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn iabs(x: ir::Value) -> Value {
    todo!()
}

/// Wrapping integer multiplication: `a := x y \pmod{2^B}`.
///
/// This instruction does not depend on the signed/unsigned interpretation
/// of the operands.
///
/// Polymorphic over all integer types (vector and scalar).
///
/// Inputs:
///
/// - x: A scalar or vector integer type
/// - y: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn imul(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Unsigned integer multiplication, producing the high half of a
/// double-length result.
///
/// Polymorphic over all integer types (vector and scalar).
///
/// Inputs:
///
/// - x: A scalar or vector integer type
/// - y: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn umulhi(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Signed integer multiplication, producing the high half of a
/// double-length result.
///
/// Polymorphic over all integer types (vector and scalar).
///
/// Inputs:
///
/// - x: A scalar or vector integer type
/// - y: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn smulhi(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Fixed-point multiplication of numbers in the QN format, where N + 1
/// is the number bitwidth:
/// `a := signed_saturate((x * y + 1 << (Q - 1)) >> Q)`
///
/// Polymorphic over all integer vector types with 16- or 32-bit numbers.
///
/// Inputs:
///
/// - x: A vector integer type with 16- or 32-bit numbers
/// - y: A vector integer type with 16- or 32-bit numbers
///
/// Outputs:
///
/// - a: A vector integer type with 16- or 32-bit numbers
fn sqmul_round_sat(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// A similar instruction to `sqmul_round_sat` except with the semantics
/// of x86's `pmulhrsw` instruction.
///
/// This is the same as `sqmul_round_sat` except when both input lanes are
/// `i16::MIN`.
///
/// Inputs:
///
/// - x: A vector integer type with 16- or 32-bit numbers
/// - y: A vector integer type with 16- or 32-bit numbers
///
/// Outputs:
///
/// - a: A vector integer type with 16- or 32-bit numbers
fn x86_pmulhrsw(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Unsigned integer division: `a := \lfloor {x \over y} \rfloor`.
///
/// This operation traps if the divisor is zero.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - y: A scalar integer type
///
/// Outputs:
///
/// - a: A scalar integer type
fn udiv(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Signed integer division rounded toward zero: `a := sign(xy)
/// \lfloor {|x| \over |y|}\rfloor`.
///
/// This operation traps if the divisor is zero, or if the result is not
/// representable in `B` bits two's complement. This only happens
/// when `x = -2^{B-1}, y = -1`.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - y: A scalar integer type
///
/// Outputs:
///
/// - a: A scalar integer type
fn sdiv(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Unsigned integer remainder.
///
/// This operation traps if the divisor is zero.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - y: A scalar integer type
///
/// Outputs:
///
/// - a: A scalar integer type
fn urem(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Signed integer remainder. The result has the sign of the dividend.
///
/// This operation traps if the divisor is zero.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - y: A scalar integer type
///
/// Outputs:
///
/// - a: A scalar integer type
fn srem(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Add immediate integer.
///
/// Same as `iadd`, but one operand is a sign extended 64 bit immediate constant.
///
/// Polymorphic over all scalar integer types, but does not support vector
/// types.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar integer type
fn iadd_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Integer multiplication by immediate constant.
///
/// Same as `imul`, but one operand is a sign extended 64 bit immediate constant.
///
/// Polymorphic over all scalar integer types, but does not support vector
/// types.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar integer type
fn imul_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Unsigned integer division by an immediate constant.
///
/// Same as `udiv`, but one operand is a zero extended 64 bit immediate constant.
///
/// This operation traps if the divisor is zero.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar integer type
fn udiv_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Signed integer division by an immediate constant.
///
/// Same as `sdiv`, but one operand is a sign extended 64 bit immediate constant.
///
/// This operation traps if the divisor is zero, or if the result is not
/// representable in `B` bits two's complement. This only happens
/// when `x = -2^{B-1}, Y = -1`.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar integer type
fn sdiv_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Unsigned integer remainder with immediate divisor.
///
/// Same as `urem`, but one operand is a zero extended 64 bit immediate constant.
///
/// This operation traps if the divisor is zero.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar integer type
fn urem_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Signed integer remainder with immediate divisor.
///
/// Same as `srem`, but one operand is a sign extended 64 bit immediate constant.
///
/// This operation traps if the divisor is zero.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar integer type
fn srem_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Immediate reverse wrapping subtraction: `a := Y - x \pmod{2^B}`.
///
/// The immediate operand is a sign extended 64 bit constant.
///
/// Also works as integer negation when `Y = 0`. Use `iadd_imm`
/// with a negative immediate operand for the reverse immediate
/// subtraction.
///
/// Polymorphic over all scalar integer types, but does not support vector
/// types.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar integer type
fn irsub_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Add integers with carry in.
///
/// Same as `iadd` with an additional carry input. Computes:
///
/// ```text
///     a = x + y + c_{in} \pmod 2^B
/// ```
///
/// Polymorphic over all scalar integer types, but does not support vector
/// types.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - y: A scalar integer type
/// - c_in: Input carry flag
///
/// Outputs:
///
/// - a: A scalar integer type
fn iadd_cin(x: ir::Value, y: ir::Value, c_in: ir::Value) -> Value {
    todo!()
}

/// Add integers with carry out.
///
/// Same as `iadd` with an additional carry output.
///
/// ```text
///     a &= x + y \pmod 2^B \\
///     c_{out} &= x+y >= 2^B
/// ```
///
/// Polymorphic over all scalar integer types, but does not support vector
/// types.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - y: A scalar integer type
///
/// Outputs:
///
/// - a: A scalar integer type
/// - c_out: Output carry flag
fn iadd_cout(x: ir::Value, y: ir::Value) -> (Value, Value) {
    todo!()
}

/// Add integers with carry in and out.
///
/// Same as `iadd` with an additional carry input and output.
///
/// ```text
///     a &= x + y + c_{in} \pmod 2^B \\
///     c_{out} &= x + y + c_{in} >= 2^B
/// ```
///
/// Polymorphic over all scalar integer types, but does not support vector
/// types.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - y: A scalar integer type
/// - c_in: Input carry flag
///
/// Outputs:
///
/// - a: A scalar integer type
/// - c_out: Output carry flag
fn iadd_carry(x: ir::Value, y: ir::Value, c_in: ir::Value) -> (Value, Value) {
    todo!()
}

/// Unsigned addition of x and y, trapping if the result overflows.
///
/// Accepts 32 or 64-bit integers, and does not support vector types.
///
/// Inputs:
///
/// - x: A 32 or 64-bit scalar integer type
/// - y: A 32 or 64-bit scalar integer type
/// - code: A trap reason code.
///
/// Outputs:
///
/// - a: A 32 or 64-bit scalar integer type
fn uadd_overflow_trap<T1: Into<ir::TrapCode>>(x: ir::Value, y: ir::Value, code: T1) -> Value {
    todo!()
}

/// Subtract integers with borrow in.
///
/// Same as `isub` with an additional borrow flag input. Computes:
///
/// ```text
///     a = x - (y + b_{in}) \pmod 2^B
/// ```
///
/// Polymorphic over all scalar integer types, but does not support vector
/// types.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - y: A scalar integer type
/// - b_in: Input borrow flag
///
/// Outputs:
///
/// - a: A scalar integer type
fn isub_bin(x: ir::Value, y: ir::Value, b_in: ir::Value) -> Value {
    todo!()
}

/// Subtract integers with borrow out.
///
/// Same as `isub` with an additional borrow flag output.
///
/// ```text
///     a &= x - y \pmod 2^B \\
///     b_{out} &= x < y
/// ```
///
/// Polymorphic over all scalar integer types, but does not support vector
/// types.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - y: A scalar integer type
///
/// Outputs:
///
/// - a: A scalar integer type
/// - b_out: Output borrow flag
fn isub_bout(x: ir::Value, y: ir::Value) -> (Value, Value) {
    todo!()
}

/// Subtract integers with borrow in and out.
///
/// Same as `isub` with an additional borrow flag input and output.
///
/// ```text
///     a &= x - (y + b_{in}) \pmod 2^B \\
///     b_{out} &= x < y + b_{in}
/// ```
///
/// Polymorphic over all scalar integer types, but does not support vector
/// types.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - y: A scalar integer type
/// - b_in: Input borrow flag
///
/// Outputs:
///
/// - a: A scalar integer type
/// - b_out: Output borrow flag
fn isub_borrow(x: ir::Value, y: ir::Value, b_in: ir::Value) -> (Value, Value) {
    todo!()
}

/// Bitwise and.
///
/// Inputs:
///
/// - x: Any integer, float, or vector type
/// - y: Any integer, float, or vector type
///
/// Outputs:
///
/// - a: Any integer, float, or vector type
fn band(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Bitwise or.
///
/// Inputs:
///
/// - x: Any integer, float, or vector type
/// - y: Any integer, float, or vector type
///
/// Outputs:
///
/// - a: Any integer, float, or vector type
fn bor(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Bitwise xor.
///
/// Inputs:
///
/// - x: Any integer, float, or vector type
/// - y: Any integer, float, or vector type
///
/// Outputs:
///
/// - a: Any integer, float, or vector type
fn bxor(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Bitwise not.
///
/// Inputs:
///
/// - x: Any integer, float, or vector type
///
/// Outputs:
///
/// - a: Any integer, float, or vector type
fn bnot(x: ir::Value) -> Value {
    todo!()
}

/// Bitwise and not.
///
/// Computes `x & ~y`.
///
/// Inputs:
///
/// - x: Any integer, float, or vector type
/// - y: Any integer, float, or vector type
///
/// Outputs:
///
/// - a: Any integer, float, or vector type
fn band_not(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Bitwise or not.
///
/// Computes `x | ~y`.
///
/// Inputs:
///
/// - x: Any integer, float, or vector type
/// - y: Any integer, float, or vector type
///
/// Outputs:
///
/// - a: Any integer, float, or vector type
fn bor_not(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Bitwise xor not.
///
/// Computes `x ^ ~y`.
///
/// Inputs:
///
/// - x: Any integer, float, or vector type
/// - y: Any integer, float, or vector type
///
/// Outputs:
///
/// - a: Any integer, float, or vector type
fn bxor_not(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Bitwise and with immediate.
///
/// Same as `band`, but one operand is a zero extended 64 bit immediate constant.
///
/// Polymorphic over all scalar integer types, but does not support vector
/// types.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar integer type
fn band_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Bitwise or with immediate.
///
/// Same as `bor`, but one operand is a zero extended 64 bit immediate constant.
///
/// Polymorphic over all scalar integer types, but does not support vector
/// types.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar integer type
fn bor_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Bitwise xor with immediate.
///
/// Same as `bxor`, but one operand is a zero extended 64 bit immediate constant.
///
/// Polymorphic over all scalar integer types, but does not support vector
/// types.
///
/// Inputs:
///
/// - x: A scalar integer type
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar integer type
fn bxor_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Rotate left.
///
/// Rotate the bits in ``x`` by ``y`` places.
///
/// Inputs:
///
/// - x: Scalar or vector value to shift
/// - y: Number of bits to shift
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn rotl(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Rotate right.
///
/// Rotate the bits in ``x`` by ``y`` places.
///
/// Inputs:
///
/// - x: Scalar or vector value to shift
/// - y: Number of bits to shift
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn rotr(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Rotate left by immediate.
///
/// Same as `rotl`, but one operand is a zero extended 64 bit immediate constant.
///
/// Inputs:
///
/// - x: Scalar or vector value to shift
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn rotl_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Rotate right by immediate.
///
/// Same as `rotr`, but one operand is a zero extended 64 bit immediate constant.
///
/// Inputs:
///
/// - x: Scalar or vector value to shift
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn rotr_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Integer shift left. Shift the bits in ``x`` towards the MSB by ``y``
/// places. Shift in zero bits to the LSB.
///
/// The shift amount is masked to the size of ``x``.
///
/// When shifting a B-bits integer type, this instruction computes:
///
/// ```text
///     s &:= y \pmod B,
///     a &:= x \cdot 2^s \pmod{2^B}.
/// ```
///
/// Inputs:
///
/// - x: Scalar or vector value to shift
/// - y: Number of bits to shift
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn ishl(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Unsigned shift right. Shift bits in ``x`` towards the LSB by ``y``
/// places, shifting in zero bits to the MSB. Also called a *logical
/// shift*.
///
/// The shift amount is masked to the size of the register.
///
/// When shifting a B-bits integer type, this instruction computes:
///
/// ```text
///     s &:= y \pmod B,
///     a &:= \lfloor x \cdot 2^{-s} \rfloor.
/// ```
///
/// Inputs:
///
/// - x: Scalar or vector value to shift
/// - y: Number of bits to shift
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn ushr(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Signed shift right. Shift bits in ``x`` towards the LSB by ``y``
/// places, shifting in sign bits to the MSB. Also called an *arithmetic
/// shift*.
///
/// The shift amount is masked to the size of the register.
///
/// Inputs:
///
/// - x: Scalar or vector value to shift
/// - y: Number of bits to shift
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn sshr(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Integer shift left by immediate.
///
/// The shift amount is masked to the size of ``x``.
///
/// Inputs:
///
/// - x: Scalar or vector value to shift
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn ishl_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Unsigned shift right by immediate.
///
/// The shift amount is masked to the size of the register.
///
/// Inputs:
///
/// - x: Scalar or vector value to shift
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn ushr_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Signed shift right by immediate.
///
/// The shift amount is masked to the size of the register.
///
/// Inputs:
///
/// - x: Scalar or vector value to shift
/// - Y: A 64-bit immediate integer.
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn sshr_imm<T1: Into<ir::immediates::Imm64>>(x: ir::Value, Y: T1) -> Value {
    todo!()
}

/// Reverse the bits of a integer.
///
/// Reverses the bits in ``x``.
///
/// Inputs:
///
/// - x: A scalar integer type
///
/// Outputs:
///
/// - a: A scalar integer type
fn bitrev(x: ir::Value) -> Value {
    todo!()
}

/// Count leading zero bits.
///
/// Starting from the MSB in ``x``, count the number of zero bits before
/// reaching the first one bit. When ``x`` is zero, returns the size of x
/// in bits.
///
/// Inputs:
///
/// - x: A scalar integer type
///
/// Outputs:
///
/// - a: A scalar integer type
fn clz(x: ir::Value) -> Value {
    todo!()
}

/// Count leading sign bits.
///
/// Starting from the MSB after the sign bit in ``x``, count the number of
/// consecutive bits identical to the sign bit. When ``x`` is 0 or -1,
/// returns one less than the size of x in bits.
///
/// Inputs:
///
/// - x: A scalar integer type
///
/// Outputs:
///
/// - a: A scalar integer type
fn cls(x: ir::Value) -> Value {
    todo!()
}

/// Count trailing zeros.
///
/// Starting from the LSB in ``x``, count the number of zero bits before
/// reaching the first one bit. When ``x`` is zero, returns the size of x
/// in bits.
///
/// Inputs:
///
/// - x: A scalar integer type
///
/// Outputs:
///
/// - a: A scalar integer type
fn ctz(x: ir::Value) -> Value {
    todo!()
}

/// Reverse the byte order of an integer.
///
/// Reverses the bytes in ``x``.
///
/// Inputs:
///
/// - x: A multi byte scalar integer type
///
/// Outputs:
///
/// - a: A multi byte scalar integer type
fn bswap(x: ir::Value) -> Value {
    todo!()
}

/// Population count
///
/// Count the number of one bits in ``x``.
///
/// Inputs:
///
/// - x: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector integer type
fn popcnt(x: ir::Value) -> Value {
    todo!()
}

/// Floating point comparison.
///
/// Two IEEE 754-2008 floating point numbers, `x` and `y`, relate to each
/// other in exactly one of four ways:
///
/// ```text
/// == ==========================================
/// UN Unordered when one or both numbers is NaN.
/// EQ When `x = y`. (And `0.0 = -0.0`).
/// LT When `x < y`.
/// GT When `x > y`.
/// == ==========================================
/// ```
///
/// The 14 `floatcc` condition codes each correspond to a subset of
/// the four relations, except for the empty set which would always be
/// false, and the full set which would always be true.
///
/// The condition codes are divided into 7 'ordered' conditions which don't
/// include UN, and 7 unordered conditions which all include UN.
///
/// ```text
/// +-------+------------+---------+------------+-------------------------+
/// |Ordered             |Unordered             |Condition                |
/// +=======+============+=========+============+=========================+
/// |ord    |EQ | LT | GT|uno      |UN          |NaNs absent / present.   |
/// +-------+------------+---------+------------+-------------------------+
/// |eq     |EQ          |ueq      |UN | EQ     |Equal                    |
/// +-------+------------+---------+------------+-------------------------+
/// |one    |LT | GT     |ne       |UN | LT | GT|Not equal                |
/// +-------+------------+---------+------------+-------------------------+
/// |lt     |LT          |ult      |UN | LT     |Less than                |
/// +-------+------------+---------+------------+-------------------------+
/// |le     |LT | EQ     |ule      |UN | LT | EQ|Less than or equal       |
/// +-------+------------+---------+------------+-------------------------+
/// |gt     |GT          |ugt      |UN | GT     |Greater than             |
/// +-------+------------+---------+------------+-------------------------+
/// |ge     |GT | EQ     |uge      |UN | GT | EQ|Greater than or equal    |
/// +-------+------------+---------+------------+-------------------------+
/// ```
///
/// The standard C comparison operators, `<, <=, >, >=`, are all ordered,
/// so they are false if either operand is NaN. The C equality operator,
/// `==`, is ordered, and since inequality is defined as the logical
/// inverse it is *unordered*. They map to the `floatcc` condition
/// codes as follows:
///
/// ```text
/// ==== ====== ============
/// C    `Cond` Subset
/// ==== ====== ============
/// `==` eq     EQ
/// `!=` ne     UN | LT | GT
/// `<`  lt     LT
/// `<=` le     LT | EQ
/// `>`  gt     GT
/// `>=` ge     GT | EQ
/// ==== ====== ============
/// ```
///
/// This subset of condition codes also corresponds to the WebAssembly
/// floating point comparisons of the same name.
///
/// When this instruction compares floating point vectors, it returns a
/// vector with the results of lane-wise comparisons.
///
/// Inputs:
///
/// - Cond: A floating point comparison condition code
/// - x: A scalar or vector floating point number
/// - y: A scalar or vector floating point number
///
/// Outputs:
///
/// - a:
fn fcmp<T1: Into<ir::condcodes::FloatCC>>(Cond: T1, x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Floating point addition.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
/// - y: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: Result of applying operator to each lane
fn fadd(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Floating point subtraction.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
/// - y: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: Result of applying operator to each lane
fn fsub(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Floating point multiplication.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
/// - y: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: Result of applying operator to each lane
fn fmul(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Floating point division.
///
/// Unlike the integer division instructions ` and
/// `udiv`, this can't trap. Division by zero is infinity or
/// NaN, depending on the dividend.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
/// - y: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: Result of applying operator to each lane
fn fdiv(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Floating point square root.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: Result of applying operator to each lane
fn sqrt(x: ir::Value) -> Value {
    todo!()
}

/// Floating point fused multiply-and-add.
///
/// Computes `a := xy+z` without any intermediate rounding of the
/// product.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
/// - y: A scalar or vector floating point number
/// - z: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: Result of applying operator to each lane
fn fma(x: ir::Value, y: ir::Value, z: ir::Value) -> Value {
    todo!()
}

/// Floating point negation.
///
/// Note that this is a pure bitwise operation.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: ``x`` with its sign bit inverted
fn fneg(x: ir::Value) -> Value {
    todo!()
}

/// Floating point absolute value.
///
/// Note that this is a pure bitwise operation.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: ``x`` with its sign bit cleared
fn fabs(x: ir::Value) -> Value {
    todo!()
}

/// Floating point copy sign.
///
/// Note that this is a pure bitwise operation. The sign bit from ``y`` is
/// copied to the sign bit of ``x``.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
/// - y: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: ``x`` with its sign bit changed to that of ``y``
fn fcopysign(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Floating point minimum, propagating NaNs using the WebAssembly rules.
///
/// If either operand is NaN, this returns NaN with an unspecified sign. Furthermore, if
/// each input NaN consists of a mantissa whose most significant bit is 1 and the rest is
/// 0, then the output has the same form. Otherwise, the output mantissa's most significant
/// bit is 1 and the rest is unspecified.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
/// - y: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: The smaller of ``x`` and ``y``
fn fmin(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Floating point pseudo-minimum, propagating NaNs.  This behaves differently from ``fmin``.
/// See <https://github.com/WebAssembly/simd/pull/122> for background.
///
/// The behaviour is defined as ``fmin_pseudo(a, b) = (b < a) ? b : a``, and the behaviour
/// for zero or NaN inputs follows from the behaviour of ``<`` with such inputs.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
/// - y: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: The smaller of ``x`` and ``y``
fn fmin_pseudo(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Floating point maximum, propagating NaNs using the WebAssembly rules.
///
/// If either operand is NaN, this returns NaN with an unspecified sign. Furthermore, if
/// each input NaN consists of a mantissa whose most significant bit is 1 and the rest is
/// 0, then the output has the same form. Otherwise, the output mantissa's most significant
/// bit is 1 and the rest is unspecified.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
/// - y: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: The larger of ``x`` and ``y``
fn fmax(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Floating point pseudo-maximum, propagating NaNs.  This behaves differently from ``fmax``.
/// See <https://github.com/WebAssembly/simd/pull/122> for background.
///
/// The behaviour is defined as ``fmax_pseudo(a, b) = (a < b) ? b : a``, and the behaviour
/// for zero or NaN inputs follows from the behaviour of ``<`` with such inputs.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
/// - y: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: The larger of ``x`` and ``y``
fn fmax_pseudo(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Round floating point round to integral, towards positive infinity.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: ``x`` rounded to integral value
fn ceil(x: ir::Value) -> Value {
    todo!()
}

/// Round floating point round to integral, towards negative infinity.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: ``x`` rounded to integral value
fn floor(x: ir::Value) -> Value {
    todo!()
}

/// Round floating point round to integral, towards zero.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: ``x`` rounded to integral value
fn trunc(x: ir::Value) -> Value {
    todo!()
}

/// Round floating point round to integral, towards nearest with ties to
/// even.
///
/// Inputs:
///
/// - x: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: ``x`` rounded to integral value
fn nearest(x: ir::Value) -> Value {
    todo!()
}

/// Reference verification.
///
/// The condition code determines if the reference type in question is
/// null or not.
///
/// Inputs:
///
/// - x: A scalar reference type
///
/// Outputs:
///
/// - a: An integer type with 8 bits.
/// WARNING: arithmetic on 8bit integers is incomplete
fn is_null(x: ir::Value) -> Value {
    todo!()
}

/// Reference verification.
///
/// The condition code determines if the reference type in question is
/// invalid or not.
///
/// Inputs:
///
/// - x: A scalar reference type
///
/// Outputs:
///
/// - a: An integer type with 8 bits.
/// WARNING: arithmetic on 8bit integers is incomplete
fn is_invalid(x: ir::Value) -> Value {
    todo!()
}

/// Reinterpret the bits in `x` as a different type.
///
/// The input and output types must be storable to memory and of the same
/// size. A bitcast is equivalent to storing one type and loading the other
/// type from the same address, both using the specified MemFlags.
///
/// Note that this operation only supports the `big` or `little` MemFlags.
/// The specified byte order only affects the result in the case where
/// input and output types differ in lane count/size.  In this case, the
/// operation is only valid if a byte order specifier is provided.
///
/// Inputs:
///
/// - MemTo (controlling type variable):
/// - MemFlags: Memory operation flags
/// - x: Any type that can be stored in memory
///
/// Outputs:
///
/// - a: Bits of `x` reinterpreted
fn bitcast<T1: Into<ir::MemFlags>>(MemTo: ir::Type, MemFlags: T1, x: ir::Value) -> Value {
    todo!()
}

/// Copies a scalar value to a vector value.  The scalar is copied into the
/// least significant lane of the vector, and all other lanes will be zero.
///
/// Inputs:
///
/// - TxN (controlling type variable): A SIMD vector type
/// - s: A scalar value
///
/// Outputs:
///
/// - a: A vector value
fn scalar_to_vector(TxN: ir::Type, s: ir::Value) -> Value {
    todo!()
}

/// Convert `x` to an integer mask.
///
/// Non-zero maps to all 1s and zero maps to all 0s.
///
/// Inputs:
///
/// - IntTo (controlling type variable): An integer type
/// - x: A scalar whose values are truthy
///
/// Outputs:
///
/// - a: An integer type
fn bmask(IntTo: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Convert `x` to a smaller integer type by discarding
/// the most significant bits.
///
/// This is the same as reducing modulo `2^n`.
///
/// Inputs:
///
/// - Int (controlling type variable): A scalar integer type
/// - x: A scalar integer type, wider than the controlling type
///
/// Outputs:
///
/// - a: A scalar integer type
fn ireduce(Int: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Combine `x` and `y` into a vector with twice the lanes but half the integer width while
/// saturating overflowing values to the signed maximum and minimum.
///
/// The lanes will be concatenated after narrowing. For example, when `x` and `y` are `i32x4`
/// and `x = [x3, x2, x1, x0]` and `y = [y3, y2, y1, y0]`, then after narrowing the value
/// returned is an `i16x8`: `a = [y3', y2', y1', y0', x3', x2', x1', x0']`.
///
/// Inputs:
///
/// - x: A SIMD vector type containing integer lanes 16, 32, or 64 bits wide
/// - y: A SIMD vector type containing integer lanes 16, 32, or 64 bits wide
///
/// Outputs:
///
/// - a:
fn snarrow(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Combine `x` and `y` into a vector with twice the lanes but half the integer width while
/// saturating overflowing values to the unsigned maximum and minimum.
///
/// Note that all input lanes are considered signed: any negative lanes will overflow and be
/// replaced with the unsigned minimum, `0x00`.
///
/// The lanes will be concatenated after narrowing. For example, when `x` and `y` are `i32x4`
/// and `x = [x3, x2, x1, x0]` and `y = [y3, y2, y1, y0]`, then after narrowing the value
/// returned is an `i16x8`: `a = [y3', y2', y1', y0', x3', x2', x1', x0']`.
///
/// Inputs:
///
/// - x: A SIMD vector type containing integer lanes 16, 32, or 64 bits wide
/// - y: A SIMD vector type containing integer lanes 16, 32, or 64 bits wide
///
/// Outputs:
///
/// - a:
fn unarrow(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Combine `x` and `y` into a vector with twice the lanes but half the integer width while
/// saturating overflowing values to the unsigned maximum and minimum.
///
/// Note that all input lanes are considered unsigned: any negative values will be interpreted as unsigned, overflowing and being replaced with the unsigned maximum.
///
/// The lanes will be concatenated after narrowing. For example, when `x` and `y` are `i32x4`
/// and `x = [x3, x2, x1, x0]` and `y = [y3, y2, y1, y0]`, then after narrowing the value
/// returned is an `i16x8`: `a = [y3', y2', y1', y0', x3', x2', x1', x0']`.
///
/// Inputs:
///
/// - x: A SIMD vector type containing integer lanes 16, 32, or 64 bits wide
/// - y: A SIMD vector type containing integer lanes 16, 32, or 64 bits wide
///
/// Outputs:
///
/// - a:
fn uunarrow(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Widen the low lanes of `x` using signed extension.
///
/// This will double the lane width and halve the number of lanes.
///
/// Inputs:
///
/// - x: A SIMD vector type containing integer lanes 8, 16, or 32 bits wide.
///
/// Outputs:
///
/// - a:
fn swiden_low(x: ir::Value) -> Value {
    todo!()
}

/// Widen the high lanes of `x` using signed extension.
///
/// This will double the lane width and halve the number of lanes.
///
/// Inputs:
///
/// - x: A SIMD vector type containing integer lanes 8, 16, or 32 bits wide.
///
/// Outputs:
///
/// - a:
fn swiden_high(x: ir::Value) -> Value {
    todo!()
}

/// Widen the low lanes of `x` using unsigned extension.
///
/// This will double the lane width and halve the number of lanes.
///
/// Inputs:
///
/// - x: A SIMD vector type containing integer lanes 8, 16, or 32 bits wide.
///
/// Outputs:
///
/// - a:
fn uwiden_low(x: ir::Value) -> Value {
    todo!()
}

/// Widen the high lanes of `x` using unsigned extension.
///
/// This will double the lane width and halve the number of lanes.
///
/// Inputs:
///
/// - x: A SIMD vector type containing integer lanes 8, 16, or 32 bits wide.
///
/// Outputs:
///
/// - a:
fn uwiden_high(x: ir::Value) -> Value {
    todo!()
}

/// Does lane-wise integer pairwise addition on two operands, putting the
/// combined results into a single vector result. Here a pair refers to adjacent
/// lanes in a vector, i.e. i*2 + (i*2+1) for i == num_lanes/2. The first operand
/// pairwise add results will make up the low half of the resulting vector while
/// the second operand pairwise add results will make up the upper half of the
/// resulting vector.
///
/// Inputs:
///
/// - x: A SIMD vector type containing integer lanes 8, 16, or 32 bits wide.
/// - y: A SIMD vector type containing integer lanes 8, 16, or 32 bits wide.
///
/// Outputs:
///
/// - a: A SIMD vector type containing integer lanes 8, 16, or 32 bits wide.
fn iadd_pairwise(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// An instruction with equivalent semantics to `pmaddubsw` on x86.
///
/// This instruction will take signed bytes from the first argument and
/// multiply them against unsigned bytes in the second argument. Adjacent
/// pairs are then added, with saturating, to a 16-bit value and are packed
/// into the result.
///
/// Inputs:
///
/// - x: A SIMD vector type consisting of 16 lanes of 8-bit integers
/// - y: A SIMD vector type consisting of 16 lanes of 8-bit integers
///
/// Outputs:
///
/// - a: A SIMD vector with exactly 8 lanes of 16-bit values
fn x86_pmaddubsw(x: ir::Value, y: ir::Value) -> Value {
    todo!()
}

/// Convert `x` to a larger integer type by zero-extending.
///
/// Each lane in `x` is converted to a larger integer type by adding
/// zeroes. The result has the same numerical value as `x` when both are
/// interpreted as unsigned integers.
///
/// The result type must have the same number of vector lanes as the input,
/// and each lane must not have fewer bits that the input lanes. If the
/// input and output types are the same, this is a no-op.
///
/// Inputs:
///
/// - Int (controlling type variable): A scalar integer type
/// - x: A scalar integer type, narrower than the controlling type
///
/// Outputs:
///
/// - a: A scalar integer type
fn uextend(Int: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Convert `x` to a larger integer type by sign-extending.
///
/// Each lane in `x` is converted to a larger integer type by replicating
/// the sign bit. The result has the same numerical value as `x` when both
/// are interpreted as signed integers.
///
/// The result type must have the same number of vector lanes as the input,
/// and each lane must not have fewer bits that the input lanes. If the
/// input and output types are the same, this is a no-op.
///
/// Inputs:
///
/// - Int (controlling type variable): A scalar integer type
/// - x: A scalar integer type, narrower than the controlling type
///
/// Outputs:
///
/// - a: A scalar integer type
fn sextend(Int: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Convert `x` to a larger floating point format.
///
/// Each lane in `x` is converted to the destination floating point format.
/// This is an exact operation.
///
/// Cranelift currently only supports two floating point formats
/// - `f32` and `f64`. This may change in the future.
///
/// The result type must have the same number of vector lanes as the input,
/// and the result lanes must not have fewer bits than the input lanes.
///
/// Inputs:
///
/// - FloatScalar (controlling type variable): A scalar only floating point number
/// - x: A scalar only floating point number, narrower than the controlling type
///
/// Outputs:
///
/// - a: A scalar only floating point number
fn fpromote(FloatScalar: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Convert `x` to a smaller floating point format.
///
/// Each lane in `x` is converted to the destination floating point format
/// by rounding to nearest, ties to even.
///
/// Cranelift currently only supports two floating point formats
/// - `f32` and `f64`. This may change in the future.
///
/// The result type must have the same number of vector lanes as the input,
/// and the result lanes must not have more bits than the input lanes.
///
/// Inputs:
///
/// - FloatScalar (controlling type variable): A scalar only floating point number
/// - x: A scalar only floating point number, wider than the controlling type
///
/// Outputs:
///
/// - a: A scalar only floating point number
fn fdemote(FloatScalar: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Convert `x` to a smaller floating point format.
///
/// Each lane in `x` is converted to the destination floating point format
/// by rounding to nearest, ties to even.
///
/// Cranelift currently only supports two floating point formats
/// - `f32` and `f64`. This may change in the future.
///
/// Fvdemote differs from fdemote in that with fvdemote it targets vectors.
/// Fvdemote is constrained to having the input type being F64x2 and the result
/// type being F32x4. The result lane that was the upper half of the input lane
/// is initialized to zero.
///
/// Inputs:
///
/// - x: A SIMD vector type consisting of 2 lanes of 64-bit floats
///
/// Outputs:
///
/// - a: A SIMD vector type consisting of 4 lanes of 32-bit floats
fn fvdemote(x: ir::Value) -> Value {
    todo!()
}

/// Converts packed single precision floating point to packed double precision floating point.
///
/// Considering only the lower half of the register, the low lanes in `x` are interpreted as
/// single precision floats that are then converted to a double precision floats.
///
/// The result type will have half the number of vector lanes as the input. Fvpromote_low is
/// constrained to input F32x4 with a result type of F64x2.
///
/// Inputs:
///
/// - a: A SIMD vector type consisting of 4 lanes of 32-bit floats
///
/// Outputs:
///
/// - x: A SIMD vector type consisting of 2 lanes of 64-bit floats
fn fvpromote_low(a: ir::Value) -> Value {
    todo!()
}

/// Converts floating point scalars to unsigned integer.
///
/// Only operates on `x` if it is a scalar. If `x` is NaN or if
/// the unsigned integral value cannot be represented in the result
/// type, this instruction traps.
///
/// Inputs:
///
/// - IntTo (controlling type variable): An scalar only integer type
/// - x: A scalar only floating point number
///
/// Outputs:
///
/// - a: An scalar only integer type
fn fcvt_to_uint(IntTo: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Converts floating point scalars to signed integer.
///
/// Only operates on `x` if it is a scalar. If `x` is NaN or if
/// the unsigned integral value cannot be represented in the result
/// type, this instruction traps.
///
/// Inputs:
///
/// - IntTo (controlling type variable): An scalar only integer type
/// - x: A scalar only floating point number
///
/// Outputs:
///
/// - a: An scalar only integer type
fn fcvt_to_sint(IntTo: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Convert floating point to unsigned integer as fcvt_to_uint does, but
/// saturates the input instead of trapping. NaN and negative values are
/// converted to 0.
///
/// Inputs:
///
/// - IntTo (controlling type variable): A larger integer type with the same number of lanes
/// - x: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: A larger integer type with the same number of lanes
fn fcvt_to_uint_sat(IntTo: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Convert floating point to signed integer as fcvt_to_sint does, but
/// saturates the input instead of trapping. NaN values are converted to 0.
///
/// Inputs:
///
/// - IntTo (controlling type variable): A larger integer type with the same number of lanes
/// - x: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: A larger integer type with the same number of lanes
fn fcvt_to_sint_sat(IntTo: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// A float-to-integer conversion instruction for vectors-of-floats which
/// has the same semantics as `cvttp{s,d}2dq` on x86. This specifically
/// returns `INT_MIN` for NaN or out-of-bounds lanes.
///
/// Inputs:
///
/// - IntTo (controlling type variable): A larger integer type with the same number of lanes
/// - x: A scalar or vector floating point number
///
/// Outputs:
///
/// - a: A larger integer type with the same number of lanes
fn x86_cvtt2dq(IntTo: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Convert unsigned integer to floating point.
///
/// Each lane in `x` is interpreted as an unsigned integer and converted to
/// floating point using round to nearest, ties to even.
///
/// The result type must have the same number of vector lanes as the input.
///
/// Inputs:
///
/// - FloatTo (controlling type variable): A scalar or vector floating point number
/// - x: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector floating point number
fn fcvt_from_uint(FloatTo: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Convert signed integer to floating point.
///
/// Each lane in `x` is interpreted as a signed integer and converted to
/// floating point using round to nearest, ties to even.
///
/// The result type must have the same number of vector lanes as the input.
///
/// Inputs:
///
/// - FloatTo (controlling type variable): A scalar or vector floating point number
/// - x: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector floating point number
fn fcvt_from_sint(FloatTo: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Converts packed signed 32-bit integers to packed double precision floating point.
///
/// Considering only the low half of the register, each lane in `x` is interpreted as a
/// signed 32-bit integer that is then converted to a double precision float. This
/// instruction differs from fcvt_from_sint in that it converts half the number of lanes
/// which are converted to occupy twice the number of bits. No rounding should be needed
/// for the resulting float.
///
/// The result type will have half the number of vector lanes as the input.
///
/// Inputs:
///
/// - FloatTo (controlling type variable): A scalar or vector floating point number
/// - x: A scalar or vector integer type
///
/// Outputs:
///
/// - a: A scalar or vector floating point number
fn fcvt_low_from_sint(FloatTo: ir::Type, x: ir::Value) -> Value {
    todo!()
}

/// Split an integer into low and high parts.
///
/// Vectors of integers are split lane-wise, so the results have the same
/// number of lanes as the input, but the lanes are half the size.
///
/// Returns the low half of `x` and the high half of `x` as two independent
/// values.
///
/// Inputs:
///
/// - x: An integer type of width `i16` upwards
///
/// Outputs:
///
/// - lo: The low bits of `x`
/// - hi: The high bits of `x`
fn isplit(x: ir::Value) -> (Value, Value) {
    todo!()
}

/// Concatenate low and high bits to form a larger integer type.
///
/// Vectors of integers are concatenated lane-wise such that the result has
/// the same number of lanes as the inputs, but the lanes are twice the
/// size.
///
/// Inputs:
///
/// - lo: An integer type of width up to `i64`
/// - hi: An integer type of width up to `i64`
///
/// Outputs:
///
/// - a: The concatenation of `lo` and `hi`
fn iconcat(lo: ir::Value, hi: ir::Value) -> Value {
    todo!()
}

/// Atomically read-modify-write memory at `p`, with second operand `x`.  The old value is
/// returned.  `p` has the type of the target word size, and `x` may be an integer type of
/// 8, 16, 32 or 64 bits, even on a 32-bit target.  The type of the returned value is the
/// same as the type of `x`.  This operation is sequentially consistent and creates
/// happens-before edges that order normal (non-atomic) loads and stores.
///
/// Inputs:
///
/// - AtomicMem (controlling type variable): Any type that can be stored in memory, which can be used in an atomic operation
/// - MemFlags: Memory operation flags
/// - AtomicRmwOp: Atomic Read-Modify-Write Ops
/// - p: An integer address type
/// - x: Value to be atomically stored
///
/// Outputs:
///
/// - a: Value atomically loaded
fn atomic_rmw<T1: Into<ir::MemFlags>, T2: Into<ir::AtomicRmwOp>>(
    AtomicMem: ir::Type,
    MemFlags: T1,
    AtomicRmwOp: T2,
    p: ir::Value,
    x: ir::Value,
) -> Value {
    todo!()
}

/// Perform an atomic compare-and-swap operation on memory at `p`, with expected value `e`,
/// storing `x` if the value at `p` equals `e`.  The old value at `p` is returned,
/// regardless of whether the operation succeeds or fails.  `p` has the type of the target
/// word size, and `x` and `e` must have the same type and the same size, which may be an
/// integer type of 8, 16, 32 or 64 bits, even on a 32-bit target.  The type of the returned
/// value is the same as the type of `x` and `e`.  This operation is sequentially
/// consistent and creates happens-before edges that order normal (non-atomic) loads and
/// stores.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - p: An integer address type
/// - e: Expected value in CAS
/// - x: Value to be atomically stored
///
/// Outputs:
///
/// - a: Value atomically loaded
fn atomic_cas<T1: Into<ir::MemFlags>>(
    MemFlags: T1,
    p: ir::Value,
    e: ir::Value,
    x: ir::Value,
) -> Value {
    todo!()
}

/// Atomically load from memory at `p`.
///
/// This is a polymorphic instruction that can load any value type which has a memory
/// representation.  It should only be used for integer types with 8, 16, 32 or 64 bits.
/// This operation is sequentially consistent and creates happens-before edges that order
/// normal (non-atomic) loads and stores.
///
/// Inputs:
///
/// - AtomicMem (controlling type variable): Any type that can be stored in memory, which can be used in an atomic operation
/// - MemFlags: Memory operation flags
/// - p: An integer address type
///
/// Outputs:
///
/// - a: Value atomically loaded
fn atomic_load<T1: Into<ir::MemFlags>>(AtomicMem: ir::Type, MemFlags: T1, p: ir::Value) -> Value {
    todo!()
}

/// Atomically store `x` to memory at `p`.
///
/// This is a polymorphic instruction that can store any value type with a memory
/// representation.  It should only be used for integer types with 8, 16, 32 or 64 bits.
/// This operation is sequentially consistent and creates happens-before edges that order
/// normal (non-atomic) loads and stores.
///
/// Inputs:
///
/// - MemFlags: Memory operation flags
/// - x: Value to be atomically stored
/// - p: An integer address type
fn atomic_store<T1: Into<ir::MemFlags>>(MemFlags: T1, x: ir::Value, p: ir::Value) -> Inst {
    todo!()
}

/// A memory fence.  This must provide ordering to ensure that, at a minimum, neither loads
/// nor stores of any kind may move forwards or backwards across the fence.  This operation
/// is sequentially consistent.
fn fence() -> Inst {
    todo!()
}

/// Return a fixed length sub vector, extracted from a dynamic vector.
///
/// Inputs:
///
/// - x: The dynamic vector to extract from
/// - y: 128-bit vector index
///
/// Outputs:
///
/// - a: New fixed vector
fn extract_vector<T1: Into<ir::immediates::Uimm8>>(x: ir::Value, y: T1) -> Value {
    todo!()
}

// /// AtomicCas(imms=(flags: ir::MemFlags), vals=3, blocks=0)
// fn AtomicCas(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     flags: ir::MemFlags,
//     arg0: Value,
//     arg1: Value,
//     arg2: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// AtomicRmw(imms=(flags: ir::MemFlags, op: ir::AtomicRmwOp), vals=2, blocks=0)
// fn AtomicRmw(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     flags: ir::MemFlags,
//     op: ir::AtomicRmwOp,
//     arg0: Value,
//     arg1: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// Binary(imms=(), vals=2, blocks=0)
// fn Binary(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     arg0: Value,
//     arg1: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// BinaryImm64(imms=(imm: ir::immediates::Imm64), vals=1, blocks=0)
// fn BinaryImm64(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     imm: ir::immediates::Imm64,
//     arg0: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// BinaryImm8(imms=(imm: ir::immediates::Uimm8), vals=1, blocks=0)
// fn BinaryImm8(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     imm: ir::immediates::Uimm8,
//     arg0: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// BranchTable(imms=(table: ir::JumpTable), vals=1, blocks=0)
// fn BranchTable(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     table: ir::JumpTable,
//     arg0: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// Brif(imms=(), vals=1, blocks=2)
// fn Brif(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     block0: ir::BlockCall,
//     block1: ir::BlockCall,
//     arg0: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// Call(imms=(func_ref: ir::FuncRef), vals=0, blocks=0)
// fn Call(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     func_ref: ir::FuncRef,
//     args: ir::ValueList,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// CallIndirect(imms=(sig_ref: ir::SigRef), vals=1, blocks=0)
// fn CallIndirect(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     sig_ref: ir::SigRef,
//     args: ir::ValueList,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// CondTrap(imms=(code: ir::TrapCode), vals=1, blocks=0)
// fn CondTrap(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     code: ir::TrapCode,
//     arg0: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// DynamicStackLoad(imms=(dynamic_stack_slot: ir::DynamicStackSlot), vals=0, blocks=0)
// fn DynamicStackLoad(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     dynamic_stack_slot: ir::DynamicStackSlot,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// DynamicStackStore(imms=(dynamic_stack_slot: ir::DynamicStackSlot), vals=1, blocks=0)
// fn DynamicStackStore(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     dynamic_stack_slot: ir::DynamicStackSlot,
//     arg0: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// FloatCompare(imms=(cond: ir::condcodes::FloatCC), vals=2, blocks=0)
// fn FloatCompare(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     cond: ir::condcodes::FloatCC,
//     arg0: Value,
//     arg1: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// FuncAddr(imms=(func_ref: ir::FuncRef), vals=0, blocks=0)
// fn FuncAddr(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     func_ref: ir::FuncRef,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// IntAddTrap(imms=(code: ir::TrapCode), vals=2, blocks=0)
// fn IntAddTrap(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     code: ir::TrapCode,
//     arg0: Value,
//     arg1: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// IntCompare(imms=(cond: ir::condcodes::IntCC), vals=2, blocks=0)
// fn IntCompare(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     cond: ir::condcodes::IntCC,
//     arg0: Value,
//     arg1: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// IntCompareImm(imms=(cond: ir::condcodes::IntCC, imm: ir::immediates::Imm64), vals=1, blocks=0)
// fn IntCompareImm(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     cond: ir::condcodes::IntCC,
//     imm: ir::immediates::Imm64,
//     arg0: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// Jump(imms=(), vals=0, blocks=1)
// fn Jump(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     block0: ir::BlockCall,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// Load(imms=(flags: ir::MemFlags, offset: ir::immediates::Offset32), vals=1, blocks=0)
// fn Load(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     flags: ir::MemFlags,
//     offset: ir::immediates::Offset32,
//     arg0: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// LoadNoOffset(imms=(flags: ir::MemFlags), vals=1, blocks=0)
// fn LoadNoOffset(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     flags: ir::MemFlags,
//     arg0: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// MultiAry(imms=(), vals=0, blocks=0)
// fn MultiAry(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     args: ir::ValueList,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// NullAry(imms=(), vals=0, blocks=0)
// fn NullAry(opcode: Opcode, ctrl_typevar: Type) -> (Inst, &'f mut ir::DataFlowGraph) {}

// /// Shuffle(imms=(imm: ir::Immediate), vals=2, blocks=0)
// fn Shuffle(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     imm: ir::Immediate,
//     arg0: Value,
//     arg1: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// StackLoad(imms=(stack_slot: ir::StackSlot, offset: ir::immediates::Offset32), vals=0, blocks=0)
// fn StackLoad(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     stack_slot: ir::StackSlot,
//     offset: ir::immediates::Offset32,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// StackStore(imms=(stack_slot: ir::StackSlot, offset: ir::immediates::Offset32), vals=1, blocks=0)
// fn StackStore(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     stack_slot: ir::StackSlot,
//     offset: ir::immediates::Offset32,
//     arg0: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// Store(imms=(flags: ir::MemFlags, offset: ir::immediates::Offset32), vals=2, blocks=0)
// fn Store(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     flags: ir::MemFlags,
//     offset: ir::immediates::Offset32,
//     arg0: Value,
//     arg1: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// StoreNoOffset(imms=(flags: ir::MemFlags), vals=2, blocks=0)
// fn StoreNoOffset(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     flags: ir::MemFlags,
//     arg0: Value,
//     arg1: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// TableAddr(imms=(table: ir::Table, offset: ir::immediates::Offset32), vals=1, blocks=0)
// fn TableAddr(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     table: ir::Table,
//     offset: ir::immediates::Offset32,
//     arg0: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// Ternary(imms=(), vals=3, blocks=0)
// fn Ternary(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     arg0: Value,
//     arg1: Value,
//     arg2: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// TernaryImm8(imms=(imm: ir::immediates::Uimm8), vals=2, blocks=0)
// fn TernaryImm8(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     imm: ir::immediates::Uimm8,
//     arg0: Value,
//     arg1: Value,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// Trap(imms=(code: ir::TrapCode), vals=0, blocks=0)
// fn Trap(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     code: ir::TrapCode,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// Unary(imms=(), vals=1, blocks=0)
// fn Unary(opcode: Opcode, ctrl_typevar: Type, arg0: Value) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// UnaryConst(imms=(constant_handle: ir::Constant), vals=0, blocks=0)
// fn UnaryConst(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     constant_handle: ir::Constant,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// UnaryGlobalValue(imms=(global_value: ir::GlobalValue), vals=0, blocks=0)
// fn UnaryGlobalValue(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     global_value: ir::GlobalValue,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// UnaryIeee32(imms=(imm: ir::immediates::Ieee32), vals=0, blocks=0)
// fn UnaryIeee32(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     imm: ir::immediates::Ieee32,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// UnaryIeee64(imms=(imm: ir::immediates::Ieee64), vals=0, blocks=0)
// fn UnaryIeee64(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     imm: ir::immediates::Ieee64,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }

// /// UnaryImm(imms=(imm: ir::immediates::Imm64), vals=0, blocks=0)
// fn UnaryImm(
//     opcode: Opcode,
//     ctrl_typevar: Type,
//     imm: ir::immediates::Imm64,
// ) -> (Inst, &'f mut ir::DataFlowGraph) {
//     todo!()
// }
