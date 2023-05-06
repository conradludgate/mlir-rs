use crate::{Context, Operation};
struct PassError {}

trait Pass {
    fn run(&self, pm: &PassManager, ctx: &Context, op: &Operation) -> Result<(), PassError>;
}

struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl PassManager {
    fn run(&self, ctx: &Context, op: &Operation) -> Result<(), PassError> {
        for pass in &self.passes {
            pass.run(self, ctx, op)?;
        }
        Ok(())
    }
}

struct RecursivePass;
impl Pass for RecursivePass {
    fn run(&self, pm: &PassManager, ctx: &Context, op: &Operation) -> Result<(), PassError> {
        let mut region = op.region_head;
        while let Some(r) = ctx.get_region(region) {
            let mut block = r.block_head;
            while let Some(b) = ctx.get_block(block) {
                let mut op = b.op_head;
                while let Some(o) = ctx.get_op(op) {
                    pm.run(ctx, o)?;
                    op = o.next;
                }
                block = b.next;
            }
            region = r.next;
        }
        Ok(())
    }
}
