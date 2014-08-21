#![crate_name = "leveldb"]
#![comment = "Bindings to LevelDB"]
#![license = "MIT"]
#![crate_type = "lib"]
// #![warn(missing_doc)]
#![warn(non_uppercase_statics)]
#![warn(managed_heap_memory)]
#![warn(unnecessary_qualification)]
#![feature(globs)]

extern crate libc;

use std::ptr;
use libc::{c_char};

use ffi::ffi as cffi;

mod ffi;


/// Error type that we get from LevelDB
pub struct LevelDBError {
    errptr: *mut c_char,
}

// TODO: drop LevelDBError
// TODO: read LevelDBError as String

pub type LevelDBResult<T> = Result<T, LevelDBError>;

// Convert a Path instance to a C-style string
fn path_as_c_str<T>(path: &Path, f: |*const i8| -> T) -> T {
    // First, convert the path to a vector...
    let mut pvec = Vec::from_slice(path.as_vec());

    // ... and ensure that it's null-terminated.
    if pvec[pvec.len() - 1] != 0 {
        pvec = pvec.append_one(0);
    }

    // Now, call the function with the new path pointer.
    // This also returns what the function does.
    f(pvec.as_ptr() as *const i8)
}

// Provides an errptr for use with LevelDB, and properly returns a Result if
// it's non-null.
fn with_errptr<T>(f: |*mut *mut c_char| -> T) -> LevelDBResult<T> {
    let mut errptr: *mut c_char = ptr::mut_null();

    let ret = f(&mut errptr as *mut *mut c_char);

    if !errptr.is_null() {
        // TODO: return error
        Err(LevelDBError {
            errptr: errptr,
        })
    } else {
        Ok(ret)
    }
}

pub struct DB {
    db: *mut cffi::leveldb_t,
}

impl DB {
    pub fn open(path: &Path) -> LevelDBResult<DB> {
        let res = path_as_c_str(path, |path| {
            with_errptr(|errptr| {
                unsafe { cffi::leveldb_open(ptr::null(), path, errptr) }
            })
        });

        let db = match res {
            Ok(db) => db,
            Err(v) => return Err(v),
        };

        Ok(DB {
            db: db,
        })
    }
}


#[cfg(test)]
mod tests {
    extern crate test;
    use super::ffi::ffi;

    #[test]
    fn test_can_get_version() {
        let major_ver = unsafe { ffi::leveldb_major_version() };
        let minor_ver = unsafe { ffi::leveldb_minor_version() };

        assert!(major_ver >= 1);
        assert!(minor_ver >= 0);
    }
}
