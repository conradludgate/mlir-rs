use crate::{Context, OpBuilder, Operation};

#[derive(Clone, Copy)]
pub enum WalkOrder {
    /// With PreOrder, the root node comes first
    PreOrder,
    /// With PostOrder, the root node comes last
    PostOrder,
    // no InOrder as we don't deal exclusively with binary trees
}

pub enum WalkResult<B> {
    /// Stop execution entirely
    Break(B),
    /// Continue deeper
    Continue,
    /// Don't descend any deeper,
    Skip,
}

impl OpBuilder<'_> {
    /// Recursively walk the operations
    pub fn walk_ops<B>(
        &self,
        order: WalkOrder,
        mut f: impl FnMut(&Operation) -> WalkResult<B>,
    ) -> WalkResult<B> {
        let op = self.ctx.get_op(self.id).unwrap();
        walk_ops(self.ctx, op, order, &mut f)
    }
}

fn walk_ops<B>(
    ctx: &Context,
    op: &Operation,
    order: WalkOrder,
    f: &mut impl FnMut(&Operation) -> WalkResult<B>,
) -> WalkResult<B> {
    if let WalkOrder::PreOrder = order {
        match f(op) {
            WalkResult::Break(b) => return WalkResult::Break(b),
            WalkResult::Skip => return WalkResult::Continue,
            WalkResult::Continue => {}
        }
    }

    let mut region = op.region_head;
    while let Some(r) = ctx.get_region(region) {
        let mut block = r.block_head;
        while let Some(b) = ctx.get_block(block) {
            let mut op = b.op_head;
            while let Some(o) = ctx.get_op(op) {
                if let WalkResult::Break(b) = walk_ops(ctx, o, order, f) {
                    return WalkResult::Break(b);
                }
                op = o.next;
            }
            block = b.next;
        }
        region = r.next;
    }

    if let WalkOrder::PostOrder = order {
        if let WalkResult::Break(b) = f(op) {
            return WalkResult::Break(b);
        }
    }

    WalkResult::Continue
}
