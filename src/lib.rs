use std::{
    cell::RefCell,
    hash::{BuildHasher, Hash, Hasher},
};

use hashbrown::{hash_map::DefaultHashBuilder, raw::RawTable};

pub struct Region<'c> {
    // blocks in this region
    blocks: &'c [Block<'c>],
}
pub struct Block<'c> {
    // name of the block
    name: InternedStr,
    // args the block requires
    args: &'c [ArgType],
    // operations the block performs
    operations: &'c [Operation<'c>],
}

pub struct ArgType {
    name: Arg,
    typ: Type,
}

struct Arg {}

pub struct Operation<'c> {
    // namespace of the dialect
    namespace: String,

    // name of the operation
    operation: &'c str,

    // name of the output SSA values
    outputs: &'c [ArgType],

    // input args
    args: &'c [ArgType],

    // additional metadata?
    attrs: Vec<Attribute>,

    // regions this operation references - eg this could be a closure body
    regions: &'c [Region<'c>],

    // where in the code does this operation come from
    location: Location,

    // control flow semantics? eg which block should follow
    successors: &'c [(InternedStr, &'c [Arg])],
}

struct Location {}

struct Type {}

struct Attribute {}

#[derive(Default)]
pub struct Context {
    interner: RefCell<Interner>,
    arena: bumpalo::Bump,
}

impl Context {
    pub fn intern(&self, str: &str) -> InternedStr {
        self.interner.borrow_mut().intern(str)
    }
}

#[derive(Default)]
pub struct Interner {
    bytes: String,
    hash_builder: DefaultHashBuilder,
    table: RawTable<InternedStr>,
}

impl Interner {
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
            let s = self.bytes.len() as u32;
            self.bytes.push_str(str);
            let e = self.bytes.len() as u32;

            *self.table.insert_entry(hash, InternedStr(s, e), |i| {
                make_hash(&self.hash_builder, &self.bytes[i.range()])
            })
        }
    }
}

#[derive(Clone, Copy)]
pub struct InternedStr(u32, u32);

impl InternedStr {
    fn range(self) -> std::ops::Range<usize> {
        self.0 as usize..self.1 as usize
    }
}
