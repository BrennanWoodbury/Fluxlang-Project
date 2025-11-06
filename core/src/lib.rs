//! Core crate entry point.
//!
//! This module wires together memory utilities, syntax structures, and
//! shared diagnostics for the rest of the FluxLang workspace.

use std::alloc::{Layout, alloc, dealloc};
use std::ptr::NonNull;

use block::{Block, BlockError, BlockPtr, BlockSize};

pub mod ast;
pub mod block;
pub mod diag;
pub mod parser;
pub mod token;
pub mod types;

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

pub fn new(size: BlockSize) -> Result<Block, BlockError> {
    if !size.is_power_of_two() {
        return Err(BlockError::BadRequest);
    }

    let new_block = Block::new(alloc_block(size)?, size)?;
    Ok(new_block)
}

pub fn alloc_block(size: BlockSize) -> Result<BlockPtr, BlockError> {
    if size == 0 || !size.is_power_of_two() {
        return Err(BlockError::BadRequest);
    }

    unsafe {
        let layout = Layout::from_size_align_unchecked(size, size);

        let ptr = alloc(layout);
        if ptr.is_null() {
            Err(BlockError::OOM)
        } else {
            Ok(NonNull::new_unchecked(ptr))
        }
    }
}

pub fn dealloc_block(ptr: BlockPtr, size: BlockSize) {
    unsafe {
        let layout = Layout::from_size_align_unchecked(size, size);

        dealloc(ptr.as_ptr(), layout);
    }
}

/* Tests */
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }

    #[test]
    fn test_alloc() {
        let size = 8;
        let block = new(size).expect("allocation falied");

        let mask = size - 1;
        let addr = block.ptr().as_ptr() as usize;
        assert_eq!(addr & mask, 0, "pointer is not {}-byte aligned", size);

        dealloc_block(block.ptr(), block.size());
    }
}
