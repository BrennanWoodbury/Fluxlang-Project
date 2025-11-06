use std::cell::UnsafeCell;
use std::fmt;
use std::marker::PhantomData;
use std::mem::size_of;

use crate::NonNull;

use crate::dealloc_block;

pub const BLOCK_SIZE_BITS: usize = 15;
pub const BLOCK_SIZE: usize = 1 << BLOCK_SIZE_BITS;
pub const LINE_SIZE_BITS: usize = 7;
pub const LINE_SIZE: usize = 1 << LINE_SIZE_BITS;
pub const LINE_COUNT: usize = BLOCK_SIZE / LINE_SIZE;
pub const BLOCK_CAPACITY: usize = BLOCK_SIZE / LINE_COUNT;
pub const ALLOC_ALIGN_MASK: usize = !(size_of::<usize>() - 1);

pub type BlockPtr = NonNull<u8>;
pub type BlockSize = usize;

pub trait AllocTypeId: Copy + Clone + PartialEq + Eq {}

pub trait AllocObject<T: AllocTypeId> {
    const TYPE_ID: T;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SizeClass {
    Small,
    Medium,
    Large,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mark {
    Unmarked,
    Marked,
}

pub type ArraySize = u32;

pub trait AllocRaw {}

// Block in memory
pub struct Block {
    ptr: BlockPtr,
    size: BlockSize,
}

#[derive(Debug, PartialEq)]
pub enum BlockError {
    BadRequest,
    OOM,
}

impl Block {
    pub fn new(ptr: BlockPtr, size: BlockSize) -> Result<Block, BlockError> {
        if !size.is_power_of_two() {
            return Err(BlockError::BadRequest);
        }
        let b = Block { ptr, size };
        Ok(b)
    }

    pub fn ptr(&self) -> BlockPtr {
        self.ptr
    }

    pub fn size(&self) -> BlockSize {
        self.size
    }

    pub fn new_guard(size: BlockSize) -> Result<BlockGuard, BlockError> {
        BlockGuard::new(size)
    }
}

// A "safe" Block -- deallocates block when it goes out of scope.
pub struct BlockGuard(Block);

impl BlockGuard {
    pub fn new(size: BlockSize) -> Result<Self, BlockError> {
        Ok(Self(crate::new(size)?))
    }

    pub fn block(&self) -> &Block {
        &self.0
    }
}

impl Drop for BlockGuard {
    fn drop(&mut self) {
        dealloc_block(self.0.ptr(), self.0.size());
    }
}

pub struct BumpBlock {
    cursor: *const u8,
    limit: *const u8,
    block: Block,
    meta: BlockMeta,
}

impl BumpBlock {
    pub fn new(block: Block, meta: BlockMeta) -> Self {
        let base = block.ptr().as_ptr();
        let limit = unsafe { base.add(BLOCK_SIZE) };

        Self {
            cursor: base,
            limit,
            block,
            meta,
        }
    }

    /// Try to allocate `alloc_size` bytes from this block.
    /// Returns a pointer to the start of the allocated region on success.
    pub fn inner_alloc(&mut self, alloc_size: usize) -> Option<*const u8> {
        // Alignment size: align to machine word
        let align = size_of::<usize>();

        // We’ll loop because if the current region fails, we’ll move to the next “hole”
        // and retry in the same function.
        let block_base = self.block.ptr().as_ptr() as usize;

        loop {
            let cursor = self.cursor as usize;
            let limit = self.limit as usize;

            // Align cursor *up* to the next multiple of `align`
            let aligned_cursor = (cursor + (align - 1)) & ALLOC_ALIGN_MASK;

            // Check for overflow and compute end of allocation
            let end = aligned_cursor.checked_add(alloc_size)?;

            if end <= limit {
                // We have enough space in the current region.
                // Allocation will start at `aligned_cursor`, and the cursor moves to `end`.
                self.cursor = end as *const u8;
                return Some(aligned_cursor as *const u8);
            }

            // Not enough space in current region. Ask BlockMeta for the next hole.
            // Compute how far into the block we are right now (cursor offset from block base).
            let current_offset = cursor.saturating_sub(block_base);

            // Ask meta for the next free [hole_start, hole_end) region (in bytes from block base).
            if let Some((hole_start, hole_end)) = self
                .meta
                .find_next_available_hole(current_offset, alloc_size)
            {
                // Convert those offsets back into absolute pointers.
                let new_cursor = block_base + hole_start;
                let new_limit = block_base + hole_end;

                self.cursor = new_cursor as *const u8;
                self.limit = new_limit as *const u8;

                // Loop again: try allocation in the new region.
                continue;
            } else {
                // No more holes big enough for this allocation in this block.
                return None;
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LineState {
    Free,
    Used,
}

pub struct BlockMeta {
    lines: [LineState; LINE_COUNT],
}

impl BlockMeta {
    pub fn new() -> Self {
        Self {
            lines: [LineState::Free; LINE_COUNT],
        }
    }

    pub fn find_next_available_hole(
        &mut self,
        start_offset: usize,
        size: usize,
    ) -> Option<(usize, usize)> {
        let start_line = start_offset / LINE_SIZE;
        let needed_lines = (size + LINE_SIZE - 1) / LINE_SIZE;

        let mut run_start = None;
        let mut run_length = 0;

        for i in start_line..LINE_COUNT {
            match self.lines[i] {
                LineState::Free => {
                    if run_start.is_none() {
                        run_start = Some(i);
                    }
                    run_length += 1;
                    if run_length >= needed_lines {
                        let start = run_start.unwrap();
                        let end = i + 1;
                        for j in start..end {
                            self.lines[j] = LineState::Used;
                        }
                        return Some((start * LINE_SIZE, end * LINE_SIZE));
                    }
                }
                LineState::Used => {
                    run_start = None;
                    run_length = 0;
                }
            }
        }
        None
    }

    pub fn free_range(&mut self, offset: usize, size: usize) {
        let start_line = offset / LINE_SIZE;
        let end_line = ((offset + size) + LINE_SIZE - 1) / LINE_SIZE;

        for i in start_line..end_line.min(LINE_COUNT) {
            self.lines[i] = LineState::Free;
        }
    }

    pub fn free_lines(&self) -> usize {
        self.lines.iter().filter(|&&l| l == LineState::Free).count()
    }

    pub unsafe fn free_object<H>(&mut self, block_base: *const u8, ptr: *const u8)
    where
        H: AllocHeader,
    {
        let obj_addr = ptr as usize;
        let base_addr = block_base as usize;
        let offset = obj_addr - base_addr;

        let header_ptr = unsafe { (ptr as *const H).sub(1) };
        let size = unsafe { (*header_ptr).size() as usize };

        self.free_range(offset, size);
    }
}

impl fmt::Debug for BlockMeta {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let free = self.free_lines();
        write!(f, "BlockMeta {{ free_lines: {} / {} }} ", free, LINE_COUNT)
    }
}

pub struct BlockList {
    head: Option<BumpBlock>,
    overflow: Option<BumpBlock>,
    rest: Vec<BumpBlock>,
    free: Option<Vec<BumpBlock>>,
    recycle: Option<Vec<BumpBlock>>,
    large: Option<Vec<BumpBlock>>,
}

pub struct StickyImmixHeap<H> {
    blocks: UnsafeCell<BlockList>,
    _header_type: PhantomData<*const H>,
}

pub trait AllocHeader: Sized {
    /// Associated type that identifies the allocated object type
    type TypeId: AllocTypeId;

    /// Create a new header for object type O
    fn new<O: AllocObject<Self::TypeId>>(size: u32, size_class: SizeClass, mark: Mark) -> Self;

    /// Create a new header for an array type
    fn new_array(size: ArraySize, size_class: SizeClass, mark: Mark) -> Self;

    /// Set the Mark value to "marked"
    fn mark(&mut self);

    /// Get the current Mark value
    fn is_marked(&self) -> bool;

    /// Get the size class of the object
    fn size_class(&self) -> SizeClass;

    /// Get the size of the object in bytes
    fn size(&self) -> u32;

    /// Get the type of the object
    fn type_id(&self) -> Self::TypeId;
}

impl<H: AllocHeader> AllocRaw for StickyImmixHeap<H> {}
