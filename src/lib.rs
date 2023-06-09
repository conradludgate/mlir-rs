#![warn(unused_crate_dependencies)]

use dialects::{Op, OpTable};
use generational_arena::{Arena, Index};

pub mod dialects;
mod display;
pub mod passes;
mod walk;

#[derive(Debug)]
pub struct Region {
    parent_op: OpId,
    next: RegionId,
    prev: RegionId,
    block_head: BlockId,
    block_tail: BlockId,
}

#[derive(Debug)]
pub struct Block {
    parent_region: RegionId,
    next: BlockId,
    prev: BlockId,
    op_head: OpId,
    op_tail: OpId,

    // args the block requires
    args: Vec<ArgType>,
}

#[derive(Debug)]
pub struct ArgType {
    pub name: Arg,
    pub typ: Type,
}

#[derive(Debug)]
pub struct Arg {}

#[derive(Debug)]
pub struct Operation {
    parent_block: BlockId,
    next: OpId,
    prev: OpId,
    region_head: RegionId,
    region_tail: RegionId,

    operation: &'static dyn Op,

    // name of the output SSA values
    outputs: Vec<ArgType>,

    // input args
    args: Vec<ArgType>,

    // additional metadata?
    attrs: Vec<Attribute>,

    // where in the code does this operation come from
    location: Location,

    // control flow semantics? eg which block should follow
    successors: Vec<(BlockId, Vec<Arg>)>,
}

#[derive(Debug)]
pub struct Location {}

#[derive(Debug)]
pub struct Type {}

#[derive(Debug)]
pub struct Attribute {}

#[derive(Default, Debug)]
pub struct Context {
    ops: Arena<Operation>,
    regions: Arena<Region>,
    blocks: Arena<Block>,
    operations: OpTable,
}

fn invalid_index() -> Index {
    Index::from_raw_parts(usize::MAX, u64::MAX)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct OpId(Index);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct BlockId(Index);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct RegionId(Index);

impl Context {
    fn get_op(&self, index: OpId) -> Option<&Operation> {
        self.ops.get(index.0)
    }
    fn get_region(&self, index: RegionId) -> Option<&Region> {
        self.regions.get(index.0)
    }
    fn get_block(&self, index: BlockId) -> Option<&Block> {
        self.blocks.get(index.0)
    }

    fn get_op_mut(&mut self, index: OpId) -> Option<&mut Operation> {
        self.ops.get_mut(index.0)
    }
    fn get_region_mut(&mut self, index: RegionId) -> Option<&mut Region> {
        self.regions.get_mut(index.0)
    }
    fn get_block_mut(&mut self, index: BlockId) -> Option<&mut Block> {
        self.blocks.get_mut(index.0)
    }
}

impl Context {
    pub fn register_op(&mut self, op: &'static dyn Op) {
        self.operations.insert(op)
    }

    pub fn module_builder(&mut self) -> BlockBuilder {
        let id = BlockId(self.blocks.insert(Block {
            parent_region: RegionId(invalid_index()),
            next: BlockId(invalid_index()),
            prev: BlockId(invalid_index()),
            op_head: OpId(invalid_index()),
            op_tail: OpId(invalid_index()),

            args: vec![],
        }));

        BlockBuilder { ctx: self, id }
    }

    pub fn reset(&mut self) {
        self.ops.clear();
        self.regions.clear();
        self.blocks.clear();
    }
}

pub struct Module<'c> {
    inner: BlockBuilder<'c>,
}

pub struct OpBuilder<'c> {
    ctx: &'c mut Context,
    id: OpId,
}

pub struct RegionBuilder<'c> {
    ctx: &'c mut Context,
    id: RegionId,
}

pub struct BlockBuilder<'c> {
    ctx: &'c mut Context,
    id: BlockId,
}

impl<'c> OpBuilder<'c> {
    pub fn add_region(self) -> RegionBuilder<'c> {
        let tail = self.ctx.get_op_mut(self.id).unwrap().region_tail;

        let id = RegionId(self.ctx.regions.insert(Region {
            parent_op: self.id,
            next: RegionId(invalid_index()),
            prev: tail,
            block_head: BlockId(invalid_index()),
            block_tail: BlockId(invalid_index()),
        }));

        if let Some(prev) = self.ctx.get_region_mut(tail) {
            prev.next = id;
            let op = self.ctx.get_op_mut(self.id).unwrap();
            op.region_tail = id;
        } else {
            let op = self.ctx.get_op_mut(self.id).unwrap();
            op.region_head = id;
            op.region_tail = id;
        }

        RegionBuilder { ctx: self.ctx, id }
    }

    pub fn finish(self) -> BlockBuilder<'c> {
        let parent = self.ctx.get_op(self.id).unwrap().parent_block;
        BlockBuilder {
            ctx: self.ctx,
            id: parent,
        }
    }
}

impl<'c> RegionBuilder<'c> {
    pub fn add_block(self) -> BlockBuilder<'c> {
        let tail = self.ctx.get_region_mut(self.id).unwrap().block_tail;

        let id = BlockId(self.ctx.blocks.insert(Block {
            parent_region: self.id,
            next: BlockId(invalid_index()),
            prev: tail,
            op_head: OpId(invalid_index()),
            op_tail: OpId(invalid_index()),

            args: vec![],
        }));

        if let Some(prev) = self.ctx.get_block_mut(tail) {
            prev.next = id;
            let region = self.ctx.get_region_mut(self.id).unwrap();
            region.block_tail = id;
        } else {
            let region = self.ctx.get_region_mut(self.id).unwrap();
            region.block_head = id;
            region.block_tail = id;
        }

        BlockBuilder { ctx: self.ctx, id }
    }

    pub fn finish(self) -> OpBuilder<'c> {
        let parent = self.ctx.get_region(self.id).unwrap().parent_op;
        OpBuilder {
            ctx: self.ctx,
            id: parent,
        }
    }
}

impl<'c> BlockBuilder<'c> {
    pub fn add_op(self, op: &'static dyn Op) -> OpBuilder<'c> {
        let tail = self.ctx.get_block_mut(self.id).unwrap().op_tail;

        self.ctx.register_op(op);

        let id = OpId(self.ctx.ops.insert(Operation {
            operation: op,
            outputs: vec![],
            args: vec![],
            attrs: vec![],
            location: Location {},
            successors: vec![],

            parent_block: self.id,
            next: OpId(invalid_index()),
            prev: tail,
            region_head: RegionId(invalid_index()),
            region_tail: RegionId(invalid_index()),
        }));

        if let Some(prev) = self.ctx.get_op_mut(tail) {
            prev.next = id;
            let op = self.ctx.get_block_mut(self.id).unwrap();
            op.op_tail = id;
        } else {
            let op = self.ctx.get_block_mut(self.id).unwrap();
            op.op_head = id;
            op.op_tail = id;
        }

        OpBuilder { ctx: self.ctx, id }
    }

    pub fn finish(self) -> Result<RegionBuilder<'c>, Module<'c>> {
        let parent = self.ctx.get_block(self.id).unwrap().parent_region;
        if self.ctx.get_region(parent).is_some() {
            Ok(RegionBuilder {
                ctx: self.ctx,
                id: parent,
            })
        } else {
            Err(Module { inner: self })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        dialects::{Dialect, Op},
        Context,
    };

    struct ToyDialect;
    impl Dialect for ToyDialect {
        fn name(&self) -> &'static str {
            "toy"
        }
    }

    struct ToyFuncOp;
    struct ToyTransposeOp;

    impl Op for ToyFuncOp {
        fn dialect(&self) -> &'static dyn crate::dialects::Dialect {
            &ToyDialect
        }

        fn name(&self) -> &'static str {
            "func"
        }

        fn verify(&self, _op: &crate::Operation) -> Result<(), crate::dialects::OpError> {
            Ok(())
        }
    }

    impl Op for ToyTransposeOp {
        fn dialect(&self) -> &'static dyn crate::dialects::Dialect {
            &ToyDialect
        }

        fn name(&self) -> &'static str {
            "transpose"
        }

        fn verify(&self, _op: &crate::Operation) -> Result<(), crate::dialects::OpError> {
            Ok(())
        }
    }

    #[test]
    fn toy_module() {
        let mut ctx = Context::default();

        let module = ctx.module_builder();
        let module = {
            let op = module.add_op(&ToyFuncOp);

            let op = {
                let region = op.add_region();

                let region = {
                    let block = region.add_block();

                    let block = block.add_op(&ToyTransposeOp).finish();

                    block.finish().unwrap()
                };

                region.finish()
            };

            op.finish()
        };
        let module = module.finish().unwrap_err();

        let module = dbg!(module);
        dbg!(module.inner.ctx);

        // let pm = PassManager::default();
    }

    struct ToyFuncOp2;

    impl Op for ToyFuncOp2 {
        fn dialect(&self) -> &'static dyn crate::dialects::Dialect {
            &ToyDialect
        }

        fn name(&self) -> &'static str {
            "func"
        }

        fn verify(&self, _op: &crate::Operation) -> Result<(), crate::dialects::OpError> {
            Ok(())
        }
    }

    #[test]
    #[should_panic(
        expected = "Operation toy.func [mlir_rs::tests::ToyFuncOp2] already registered by a different type [mlir_rs::tests::ToyFuncOp]"
    )]
    fn toy_module_dupe_op() {
        let mut ctx = Context::default();
        ctx.register_op(&ToyFuncOp);
        ctx.register_op(&ToyFuncOp2);
    }

    #[test]
    fn toy_module_dupe_same_op() {
        let mut ctx = Context::default();
        ctx.register_op(&ToyFuncOp);
        ctx.register_op(&ToyFuncOp);
    }
}
