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

use ffi::{leveldb_major_version, leveldb_minor_version};
mod ffi;



#[cfg(test)]
mod tests {
    #[test]
    fn test_can_get_version() {
        let major_ver = unsafe { ffi::leveldb_major_version() };
        let minor_ver = unsafe { ffi::leveldb_minor_version() };

        assert!(major_ver >= 1);
        assert!(minor_ver >= 0);
    }
}
