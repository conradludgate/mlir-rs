use crate::{Context, Operation};
pub struct PassError {}

pub trait Pass {
    fn run(&self, ctx: &Context, op: &Operation) -> Result<(), PassError>;
}

#[derive(Default)]
pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl PassManager {
    pub fn run(&self, ctx: &Context, op: &Operation) -> Result<(), PassError> {
        for pass in &self.passes {
            pass.run(ctx, op)?;
        }
        Ok(())
    }

    pub fn add_pass(&mut self, p: Box<impl Pass + 'static>) {
        self.passes.push(p)
    }
}

#[derive(Default)]
pub struct NestedPassManager {
    pm: PassManager,
}

impl NestedPassManager {
    pub fn add_pass(&mut self, p: Box<impl Pass + 'static>) {
        self.pm.add_pass(p)
    }
}

impl Pass for NestedPassManager {
    fn run(&self, ctx: &Context, op: &Operation) -> Result<(), PassError> {
        let mut region = op.region_head;
        while let Some(r) = ctx.get_region(region) {
            let mut block = r.block_head;
            while let Some(b) = ctx.get_block(block) {
                let mut op = b.op_head;
                while let Some(o) = ctx.get_op(op) {
                    self.run(ctx, o)?;
                    op = o.next;
                }
                block = b.next;
            }
            region = r.next;
        }
        Ok(())
    }
}

pub trait Convert {
    type State;

    /// determine if this convert applies to the operation. If it does, return `Some(state)`
    fn find(&self, ctx: &Context, op: &Operation) -> Option<Self::State>;
    /// apply the rewrite
    fn rewrite(&self, ctx: &mut Context, op: &Operation, state: Self::State);
}
