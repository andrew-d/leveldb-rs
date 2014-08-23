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
use libc::{c_char, c_uchar, c_void};
use libc::types::os::arch::c95::size_t;

use ffi::ffi as cffi;

mod ffi;


/// Error type that we get from LevelDB
pub struct LevelDBError {
    errptr: *mut c_char,
}

impl LevelDBError {
    /// Return the error as a string.
    pub fn as_string(&self) -> String {
        unsafe {
            std::string::raw::from_buf(self.errptr as *const u8)
        }
    }
}

impl std::fmt::Show for LevelDBError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::FormatError> {
        self.as_string().fmt(f)
    }
}

impl Drop for LevelDBError {
    fn drop(&mut self) {
        unsafe { cffi::leveldb_free(self.errptr as *mut c_void) };
    }
}

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
        Err(LevelDBError {
            errptr: errptr,
        })
    } else {
        Ok(ret)
    }
}

pub fn bool_to_uchar(val: bool) -> c_uchar {
    if val {
        1 as c_uchar
    } else {
        0 as c_uchar
    }
}

/**
 * This structure represents options that can be used when constructing a
 * LevelDB instance.
 */
pub struct DBOptions {
    opts: *mut cffi::leveldb_options_t,
}

impl DBOptions {
    /**
     * Create and return a new DBOptions instance.  Returns `None` if the
     * underlying library call returns a null pointer.
     */
    pub fn new() -> Option<DBOptions> {
        let opts = unsafe { cffi::leveldb_options_create() };
        if opts.is_null() {
            None
        } else {
            Some(DBOptions {
                opts: opts,
            })
        }
    }

    /**
     * Create the database if it's missing when we try to open it.
     */
    pub fn create_if_missing(&mut self, val: bool) {
        unsafe {
            cffi::leveldb_options_set_create_if_missing(self.opts, bool_to_uchar(val));
        }
    }

    unsafe fn ptr(&self) -> *const cffi::leveldb_options_t {
        self.opts as *const cffi::leveldb_options_t
    }
}

impl Drop for DBOptions {
    fn drop(&mut self) {
        unsafe { cffi::leveldb_options_destroy(self.opts) };
    }
}

/**
 * This structure represents options that can be used when writing to a LevelDB
 * instance.
 */
pub struct DBWriteOptions {
    opts: *mut cffi::leveldb_writeoptions_t,
}

impl DBWriteOptions {
    /**
     * Create and return a new DBWriteOptions instance.  Returns `None` if the
     * underlying library call returns a null pointer.
     */
    pub fn new() -> Option<DBWriteOptions> {
        let opts = unsafe { cffi::leveldb_writeoptions_create() };
        if opts.is_null() {
            None
        } else {
            Some(DBWriteOptions {
                opts: opts,
            })
        }
    }

    unsafe fn ptr(&self) -> *const cffi::leveldb_writeoptions_t {
        self.opts as *const cffi::leveldb_writeoptions_t
    }
}

impl Drop for DBWriteOptions {
    fn drop(&mut self) {
        unsafe { cffi::leveldb_writeoptions_destroy(self.opts) };
    }
}

/**
 * This struct represents an open instance of the database.
 */
pub struct DB {
    db: *mut cffi::leveldb_t,
}

impl DB {
    /**
     * Open a database at the given path.  Returns a Result indicating whether
     * the database could be opened.  Note that this function will not create
     * the database at the given location if it does not exist.
     */
    pub fn open(path: &Path) -> LevelDBResult<DB> {
        // TODO: proper return code for OOM
        let opts = match DBOptions::new() {
            Some(o) => o,
            None    => fail!("Out of memory"),
        };

        DB::open_with_opts(path, opts)
    }

    /**
     * Create and returns a database at the given path.
     */
    pub fn create(path: &Path) -> LevelDBResult<DB> {
        // TODO: proper return code for OOM
        let mut opts = match DBOptions::new() {
            Some(o) => o,
            None    => fail!("Out of memory"),
        };

        // TODO: can we remove a previously-existing database?

        opts.create_if_missing(true);
        DB::open_with_opts(path, opts)
    }

    /**
     * Open a database at the given path, using the provided options to control
     * the open behaviour.  Returns a Result indicating whether or not the
     * database could be opened.
     */
    pub fn open_with_opts(path: &Path, opts: DBOptions) -> LevelDBResult<DB> {
        let res = path_as_c_str(path, |path| {
            with_errptr(|errptr| {
                unsafe { cffi::leveldb_open(opts.ptr(), path, errptr) }
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

    /**
     * Set the database entry for "key" to "value". Returns a result indicating
     * the success or failure of the operation.
     */
    pub fn put(&mut self, key: &[u8], val: &[u8]) -> LevelDBResult<()> {
        // TODO: proper return code for OOM
        let opts = match DBWriteOptions::new() {
            Some(o) => o,
            None    => fail!("Out of memory"),
        };

        try!(with_errptr(|errptr| {
            unsafe {
                cffi::leveldb_put(
                    self.db,
                    opts.ptr(),
                    key.as_ptr() as *const c_char,
                    key.len() as size_t,
                    val.as_ptr() as *const c_char,
                    val.len() as size_t,
                    errptr
                )
            }
        }))

        Ok(())
    }
}

impl Drop for DB {
    fn drop(&mut self) {
        unsafe {
            cffi::leveldb_close(self.db)
        }
    }
}


#[cfg(test)]
mod tests {
    extern crate test;

    use std::io::TempDir;

    use super::DB;
    use super::ffi::ffi;

    #[test]
    fn test_can_get_version() {
        let major_ver = unsafe { ffi::leveldb_major_version() };
        let minor_ver = unsafe { ffi::leveldb_minor_version() };

        assert!(major_ver >= 1);
        assert!(minor_ver >= 0);
    }

    #[test]
    fn test_can_create() {
        let tdir = match TempDir::new("create") {
            Some(t) => t,
            None    => fail!("Error creating temp dir"),
        };

        let _db = match DB::create(tdir.path()) {
            Ok(db)   => db,
            Err(why) => fail!("Error creating DB: {}", why),
        };
    }

    #[test]
    fn test_put() {
        let tdir = match TempDir::new("put") {
            Some(t) => t,
            None    => fail!("Error creating temp dir"),
        };

        let mut db = match DB::create(tdir.path()) {
            Ok(db)   => db,
            Err(why) => fail!("Error creating DB: {}", why),
        };

        match db.put(b"foo", b"bar") {
            Ok(_)    => {},
            Err(why) => fail!("Error putting into DB: {}", why),
        };
    }
}
