use std::{any::Any, fmt};

use hashbrown::{hash_map::DefaultHashBuilder, raw::RawTable};

use crate::Operation;

pub mod cranelift;

pub trait Dialect {
    fn name(&self) -> &'static str;
}

/// Meant to represent a helper to define operations.
pub trait Op {
    fn dialect(&self) -> &'static dyn Dialect;
    fn name(&self) -> &'static str;

    fn verify(&self, op: &Operation) -> Result<(), OpError>;
}

pub struct OpError {}

#[derive(Default)]
pub(crate) struct OpTable {
    hash_builder: DefaultHashBuilder,
    table: RawTable<&'static dyn Op>,
}

impl OpTable {
    pub(crate) fn insert(&mut self, op: &'static dyn Op) {
        use std::hash::{BuildHasher, Hash, Hasher};
        fn make_hash(hash_builder: &DefaultHashBuilder, op: &dyn Op) -> u64 {
            let mut state = hash_builder.build_hasher();
            op.dialect().name().hash(&mut state);
            op.name().hash(&mut state);
            state.finish()
        }
        let hash = make_hash(&self.hash_builder, op);
        if let Some(&op2) = self.table.get(hash, |&op2| {
            op2.dialect().name() == op.dialect().name() && op2.name() == op.name()
        }) {
            if op2.type_id() != op.type_id() {
                dbg!(op2.type_id(), op.type_id());
                panic!(
                    "Operation {}.{} already registered by a different type",
                    op.dialect().name(),
                    op.name(),
                )
            }
        } else {
            self.table
                .insert_entry(hash, op, |&op2| make_hash(&self.hash_builder, op2));
        }
    }
}

impl fmt::Debug for OpTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_tuple("OpTable");
        // Safety: we don't dealloc the table while iterating and we don't remove any buckets
        unsafe {
            for op in self.table.iter() {
                f.field(op.as_ref());
            }
        }
        f.finish()
    }
}

impl fmt::Debug for dyn Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.dialect().name(), self.name())
    }
}
