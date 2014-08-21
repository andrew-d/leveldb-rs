#![feature(globs)]

use ffi::*;

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
