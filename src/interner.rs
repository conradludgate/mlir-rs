use std::{
    fmt,
    hash::{BuildHasher, Hash, Hasher},
};

use hashbrown::{hash_map::DefaultHashBuilder, raw::RawTable};

#[derive(Default)]
pub struct Interner {
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
