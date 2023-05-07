use std::fmt;

use crate::{BlockBuilder, BlockId, Context, Module, OpBuilder, OpId, RegionBuilder, RegionId};

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
