use std::{
    fmt,
    hash::{BuildHasher, Hash, Hasher},
};

use generational_arena::{Arena, Index};
use hashbrown::{hash_map::DefaultHashBuilder, raw::RawTable};

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

    // name of the block
    name: InternedStr,
    // args the block requires
    args: Vec<ArgType>,
}

#[derive(Debug)]
pub struct ArgType {
    pub name: Arg,
    pub typ: Type,
}

#[derive(Debug)]
pub struct Arg {
    pub name: InternedStr,
}

#[derive(Debug)]
pub struct Operation {
    parent_block: BlockId,
    next: OpId,
    prev: OpId,
    region_head: RegionId,
    region_tail: RegionId,

    // namespace of the dialect
    namespace: InternedStr,

    // name of the operation
    operation: InternedStr,

    // name of the output SSA values
    outputs: Vec<ArgType>,

    // input args
    args: Vec<ArgType>,

    // additional metadata?
    attrs: Vec<Attribute>,

    // where in the code does this operation come from
    location: Location,

    // control flow semantics? eg which block should follow
    successors: Vec<(InternedStr, Vec<Arg>)>,
}

#[derive(Debug)]
pub struct Location {}

#[derive(Debug)]
pub struct Type {}

#[derive(Debug)]
pub struct Attribute {}

#[derive(Default, Debug)]
pub struct Context {
    interner: Interner,
    ops: Arena<Operation>,
    regions: Arena<Region>,
    blocks: Arena<Block>,
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
    pub fn intern(&mut self, str: &str) -> InternedStr {
        self.interner.intern(str)
    }

    pub fn module_builder(&mut self, name: InternedStr) -> BlockBuilder {
        let id = BlockId(self.blocks.insert(Block {
            parent_region: RegionId(invalid_index()),
            next: BlockId(invalid_index()),
            prev: BlockId(invalid_index()),
            op_head: OpId(invalid_index()),
            op_tail: OpId(invalid_index()),

            name,
            args: vec![],
        }));

        BlockBuilder { ctx: self, id }
    }

    pub fn reset(&mut self) {
        self.interner.clear();
        self.ops.clear();
        self.regions.clear();
        self.blocks.clear();
    }
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
    pub fn intern(&mut self, str: &str) -> InternedStr {
        self.ctx.intern(str)
    }

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
    pub fn intern(&mut self, str: &str) -> InternedStr {
        self.ctx.intern(str)
    }

    pub fn add_block(self, name: InternedStr) -> BlockBuilder<'c> {
        let tail = self.ctx.get_region_mut(self.id).unwrap().block_tail;

        let id = BlockId(self.ctx.blocks.insert(Block {
            parent_region: self.id,
            next: BlockId(invalid_index()),
            prev: tail,
            op_head: OpId(invalid_index()),
            op_tail: OpId(invalid_index()),

            name,
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
    pub fn intern(&mut self, str: &str) -> InternedStr {
        self.ctx.intern(str)
    }

    pub fn add_op(self, ns: InternedStr, op: InternedStr) -> OpBuilder<'c> {
        let tail = self.ctx.get_block_mut(self.id).unwrap().op_tail;

        let id = OpId(self.ctx.ops.insert(Operation {
            namespace: ns,
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
#[derive(Default)]
struct Interner {
    bytes: String,
    hash_builder: DefaultHashBuilder,
    table: RawTable<InternedStr>,
}

impl fmt::Debug for Interner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Interner").finish_non_exhaustive()
    }
}

impl Interner {
    pub fn clear(&mut self) {
        self.bytes.clear();
        self.table.clear_no_drop();
    }

    pub fn get(&self, i: InternedStr) -> Option<&str> {
        self.bytes.get(i.range())
    }

    pub fn intern(&mut self, str: &str) -> InternedStr {
        fn make_hash(hash_builder: &DefaultHashBuilder, str: &str) -> u64 {
            let mut state = hash_builder.build_hasher();
            str.hash(&mut state);
            state.finish()
        }

        let hash = make_hash(&self.hash_builder, str);
        if let Some(i) = self.table.get(hash, |i| &self.bytes[i.range()] == str) {
            *i
        } else {
            let s = u32::try_from(self.bytes.len())
                .expect("interned string should not exceed u32::MAX");
            self.bytes.push_str(str);
            let e = u32::try_from(self.bytes.len())
                .expect("interned string should not exceed u32::MAX");

            *self.table.insert_entry(hash, InternedStr(s, e), |i| {
                make_hash(&self.hash_builder, &self.bytes[i.range()])
            })
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct InternedStr(u32, u32);

impl InternedStr {
    fn range(self) -> std::ops::Range<usize> {
        self.0 as usize..self.1 as usize
    }
}

#[cfg(test)]
mod tests {
    use crate::Context;

    #[test]
    fn toy_module() {
        let mut ctx = Context::default();

        let toy = ctx.intern("toy");

        let mut module = ctx.module_builder(toy);
        let module = {
            let func = module.intern("func");
            let op = module.add_op(toy, func);

            let op = {
                let mut region = op.add_region();

                let region = {
                    let bb0 = region.intern("bb0");
                    let mut block = region.add_block(bb0);
                    let transpose = block.intern("transpose");

                    let block = block.add_op(toy, transpose).finish();

                    block.finish().unwrap()
                };

                region.finish()
            };

            op.finish()
        };
        let module = module.finish().unwrap_err();

        dbg!(module);
    }
}

pub struct Module<'c> {
    inner: BlockBuilder<'c>,
}

impl fmt::Debug for Module<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        BlockDisplay {
            ctx: self.inner.ctx,
            id: self.inner.id,
        }
        .fmt(f)
    }
}

impl fmt::Debug for BlockBuilder<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        BlockDisplay {
            ctx: self.ctx,
            id: self.id,
        }
        .fmt(f)
    }
}

impl fmt::Debug for RegionBuilder<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        RegionDisplay {
            ctx: self.ctx,
            id: self.id,
        }
        .fmt(f)
    }
}
impl fmt::Debug for OpBuilder<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        OpDisplay {
            ctx: self.ctx,
            id: self.id,
        }
        .fmt(f)
    }
}

struct BlockDisplay<'c> {
    ctx: &'c Context,
    id: BlockId,
}
struct RegionsDisplay<'c> {
    ctx: &'c Context,
    id: RegionId,
}
struct RegionDisplay<'c> {
    ctx: &'c Context,
    id: RegionId,
}
struct OpDisplay<'c> {
    ctx: &'c Context,
    id: OpId,
}

impl fmt::Debug for BlockDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let block = self.ctx.get_block(self.id).unwrap();
        let mut f = f.debug_tuple(self.ctx.interner.get(block.name).unwrap());

        let mut op = block.op_head;
        while let Some(o) = self.ctx.get_op(op) {
            f.field(&OpDisplay {
                ctx: self.ctx,
                id: op,
            });
            op = o.next;
        }

        f.finish()
    }
}

impl fmt::Debug for RegionsDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_list();

        let mut region = self.id;
        while let Some(r) = self.ctx.get_region(region) {
            f.entry(&RegionDisplay {
                ctx: self.ctx,
                id: region,
            });
            region = r.next;
        }

        f.finish()
    }
}

impl fmt::Debug for RegionDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let region = self.ctx.get_region(self.id).unwrap();
        let mut f = f.debug_list();

        let mut block = region.block_head;
        while let Some(b) = self.ctx.get_block(block) {
            f.entry(&BlockDisplay {
                ctx: self.ctx,
                id: block,
            });
            block = b.next;
        }

        f.finish()
    }
}

impl fmt::Debug for OpDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = self.ctx.get_op(self.id).unwrap();
        let mut f = f.debug_struct(&format!(
            "{}.{}",
            self.ctx.interner.get(op.namespace).unwrap(),
            self.ctx.interner.get(op.operation).unwrap()
        ));

        f.field(
            "regions",
            &RegionsDisplay {
                ctx: self.ctx,
                id: op.region_head,
            },
        );

        f.finish()
    }
}
