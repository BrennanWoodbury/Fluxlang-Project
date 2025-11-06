//! Simple arena allocator for HIR data structures.
//!
//! The arena provides stable indices so semantic passes can refer to HIR nodes
//! without cloning large structures. The implementation is intentionally
//! minimal and will evolve alongside the requirements of later phases.

use std::cell::{Ref, RefCell, RefMut};
use std::marker::PhantomData;

/// Stable index into an [`Arena`].
pub struct ArenaIndex<T> {
    index: usize,
    _marker: PhantomData<T>,
}

impl<T> ArenaIndex<T> {
    /// Construct a new index from a raw position.
    pub const fn from_raw(index: usize) -> Self {
        Self {
            index,
            _marker: PhantomData,
        }
    }

    /// Retrieve the raw index.
    pub const fn to_raw(self) -> usize {
        self.index
    }
}

/// Basic bump-allocator-style arena for storing HIR nodes.
#[derive(Debug, Default)]
pub struct Arena<T> {
    entries: RefCell<Vec<T>>,
}

impl<T> Arena<T> {
    /// Create an empty arena.
    pub fn new() -> Self {
        Self {
            entries: RefCell::new(Vec::new()),
        }
    }

    /// Allocate a new value in the arena and return its index.
    pub fn alloc(&self, value: T) -> ArenaIndex<T> {
        let mut entries = self.entries.borrow_mut();
        let index = entries.len();
        entries.push(value);
        ArenaIndex::from_raw(index)
    }

    /// Borrow an immutable reference to the value stored at `index`.
    pub fn get(&self, index: ArenaIndex<T>) -> Option<Ref<'_, T>> {
        let raw = index.to_raw();
        let len = self.entries.borrow().len();
        if raw >= len {
            return None;
        }

        // Split borrow to avoid holding mutable reference in the caller.
        let entries = self.entries.borrow();
        Some(Ref::map(entries, |items| &items[raw]))
    }

    /// Borrow a mutable reference to the value stored at `index`.
    pub fn get_mut(&self, index: ArenaIndex<T>) -> Option<RefMut<'_, T>> {
        let raw = index.to_raw();
        let len = self.entries.borrow().len();
        if raw >= len {
            return None;
        }

        let entries = self.entries.borrow_mut();
        Some(RefMut::map(entries, |items| &mut items[raw]))
    }

    /// Iterate over the stored entries.
    pub fn iter(&self) -> Vec<Ref<'_, T>> {
        let len = self.entries.borrow().len();
        (0..len)
            .filter_map(|idx| self.get(ArenaIndex::from_raw(idx)))
            .collect()
    }

    /// Number of elements currently allocated in the arena.
    pub fn len(&self) -> usize {
        self.entries.borrow().len()
    }

    /// Whether the arena contains no elements.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
