/*!
 * Rust bindings for [LevelDB](https://code.google.com/p/leveldb/), a fast and
 * lightweight key/value database library from Google.
 */
#![crate_name = "leveldb"]
#![comment = "Bindings to LevelDB"]
#![license = "MIT"]
#![crate_type = "lib"]
#![warn(missing_doc)]
#![warn(non_uppercase_statics)]
#![warn(managed_heap_memory)]
#![warn(unnecessary_qualification)]
#![feature(globs)]
#![feature(unsafe_destructor)]

extern crate libc;

use std::ptr;
use std::raw::Slice;
use std::mem::transmute;

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

fn bool_to_uchar(val: bool) -> c_uchar {
    if val {
        1 as c_uchar
    } else {
        0 as c_uchar
    }
}

fn uchar_to_bool(val: c_uchar) -> bool {
    if val == 0 {
        false
    } else {
        true
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
 * This structure represents options that can be used when reading from a
 * LevelDB instance.
 */
pub struct DBReadOptions {
    opts: *mut cffi::leveldb_readoptions_t,
}

impl DBReadOptions {
    /**
     * Create and return a new DBReadOptions instance.  Returns `None` if the
     * underlying library call returns a null pointer.
     */
    pub fn new() -> Option<DBReadOptions> {
        let opts = unsafe { cffi::leveldb_readoptions_create() };
        if opts.is_null() {
            None
        } else {
            Some(DBReadOptions {
                opts: opts,
            })
        }
    }

    /**
     * If set to 'true', all data read from the underlying storage will be
     * verified against corresponding checksums.
     *
     * Defaults to 'false'.
     */
    pub fn set_verify_checksums(&mut self, val: bool) -> &mut DBReadOptions {
        unsafe {
            cffi::leveldb_readoptions_set_verify_checksums(self.opts, bool_to_uchar(val));
        }

        self
    }

    /**
     * Set whether the data read for this iteration should be cached in memory.
     *
     * Defaults to 'true'.
     */
    pub fn set_fill_cache(&mut self, val: bool) -> &mut DBReadOptions {
        unsafe {
            cffi::leveldb_readoptions_set_fill_cache(self.opts, bool_to_uchar(val));
        }

        self
    }

    /**
     * Set the snapshot to use when reading from the database.  If this is not
     * set, then an implicit snapshot - of the state as of the beginning of the
     * read operation - will be used.
     */
    pub fn set_snapshot(&mut self, _snap: &DBSnapshot) -> &mut DBReadOptions {
        // TODO:

        self
    }

    unsafe fn ptr(&self) -> *const cffi::leveldb_readoptions_t {
        self.opts as *const cffi::leveldb_readoptions_t
    }
}

impl Drop for DBReadOptions {
    fn drop(&mut self) {
        unsafe { cffi::leveldb_readoptions_destroy(self.opts) };
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

    /**
     * Set whether the write will be flushed to disk before the write is
     * considered "complete".  Essentially, if a write is performed without
     * this value set, it has the same semantics as the `write()` syscall.  If
     * sync is set, the semantics are the same as a `write()` followed by a
     * `fsync()` call.
     *
     * The default value is false.
     */
    pub fn set_sync(&mut self, val: bool) -> &mut DBWriteOptions {
        unsafe {
            cffi::leveldb_writeoptions_set_sync(self.opts, bool_to_uchar(val));
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
 * A write batch holds a collection of updates to apply atomically to a
 * database.  Updates are applied in the order in which they are added to the
 * write batch.
 */
pub struct DBWriteBatch {
    batch: *mut cffi::leveldb_writebatch_t,
}

impl DBWriteBatch {
    /**
     * Create a new, empty write batch.  Returns None if the underlying library
     * call returns a null pointer.
     */
    pub fn new() -> Option<DBWriteBatch> {
        let batch = unsafe { cffi::leveldb_writebatch_create() };
        if batch.is_null() {
            None
        } else {
            Some(DBWriteBatch {
                batch: batch,
            })
        }
    }

    /**
     * Set the database entry for "key" to "value".  See `put()` on `DB` for
     * more information.
     */
    pub fn put(&mut self, key: &[u8], val: &[u8]) {
        // TODO: does the API copy the underlying key/value, or do we need to
        // ensure it lives long enough?
        unsafe {
            cffi::leveldb_writebatch_put(
                self.batch,
                key.as_ptr() as *const c_char,
                key.len() as size_t,
                val.as_ptr() as *const c_char,
                val.len() as size_t
            )
        }
    }

    /**
     * Clear all updates buffered in this write batch.
     */
    pub fn clear(&mut self) {
        unsafe { cffi::leveldb_writebatch_clear(self.batch) };
    }

    /**
     * If the database contains the given key, erase it.  Otherwise, do
     * nothing.
     */
    pub fn delete(&mut self, key: &[u8]) {
        unsafe {
            cffi::leveldb_writebatch_delete(
                self.batch,
                key.as_ptr() as *const c_char,
                key.len() as size_t
            )
        };
    }
}

impl Drop for DBWriteBatch {
    fn drop(&mut self) {
        unsafe { cffi::leveldb_writebatch_destroy(self.batch) };
    }
}

/**
 * This structure represents an iterator over the database.  Note that since
 * the next() function is bounded by a lifetime, it does not (quite) conform
 * to the Iterator trait.  To get this, use the alloc() helper.
 */
pub struct DBIterator {
    iter: *mut cffi::leveldb_iterator_t,
}

impl DBIterator {
    // Note: deliberately not public
    fn new(i: *mut cffi::leveldb_iterator_t) -> DBIterator {
        unsafe { cffi::leveldb_iter_seek_to_first(i) };

        DBIterator {
            iter: i,
        }
    }

    /**
     * Return the next key/value pair from this iterator.
     */
    pub fn next<'a>(&'a mut self) -> Option<(&'a [u8], &'a [u8])> {
        if !uchar_to_bool(unsafe { cffi::leveldb_iter_valid(self.ptr()) }) {
            return None;
        }

        let key_slice = unsafe {
            let mut keylen: size_t = 0;
            let key = cffi::leveldb_iter_key(self.ptr(),
                &mut keylen as *mut size_t);

            transmute(Slice {
                data: key,
                len:  keylen as uint,
            })
        };

        let val_slice = unsafe {
            let mut vallen: size_t = 0;
            let val = cffi::leveldb_iter_value(
                self.ptr(), &mut vallen as *mut size_t);

            transmute(Slice {
                data: val,
                len:  vallen as uint,
            })
        };

        unsafe { cffi::leveldb_iter_next(self.iter) };

        Some((key_slice, val_slice))
    }

    /**
     * Return an instance of DBIteratorAlloc, an iterator that implements the
     * Iterator trait, but allocates new Vec<u8>s for each item.  Note that
     * this consumes the DBIterator instance, so it can't be used again.
     */
    pub fn alloc(self) -> DBIteratorAlloc {
        DBIteratorAlloc::new(self)
    }

    /**
     * Seek to the beginning of the database.
     */
    pub fn seek_to_first(&mut self) {
        unsafe { cffi::leveldb_iter_seek_to_first(self.iter) };
    }

    /**
     * Seek to the end of the database.
     */
    pub fn seek_to_last(&mut self) {
        unsafe { cffi::leveldb_iter_seek_to_last(self.iter) };
    }

    /**
     * Seek to the first key in the database that is at or past the given
     * target key.
     */
    pub fn seek(&mut self, key: &[u8]) {
        unsafe {
            cffi::leveldb_iter_seek(
                self.iter,
                key.as_ptr() as *const c_char,
                key.len() as size_t
            );
        }
    }

    fn ptr(&self) -> *const cffi::leveldb_iterator_t {
        self.iter as *const cffi::leveldb_iterator_t
    }
}

impl Drop for DBIterator {
    fn drop(&mut self) {
        unsafe { cffi::leveldb_iter_destroy(self.iter) };
    }
}

/**
 * An iterator over a database that implements the standard library's Iterator
 * trait.
 */
pub struct DBIteratorAlloc {
    underlying: DBIterator,
}

impl DBIteratorAlloc {
    // Note: deliberately not public
    fn new(i: DBIterator) -> DBIteratorAlloc {
        DBIteratorAlloc {
            underlying: i,
        }
    }

    /**
     * Wraps the underlying `seek_to_first` call.
     */
    pub fn seek_to_first(&mut self) {
        self.underlying.seek_to_first()
    }

    /**
     * Wraps the underlying `seek_to_last` call.
     */
    pub fn seek_to_last(&mut self) {
        self.underlying.seek_to_last()
    }

    /**
     * Wrap the underlying `seek` call.
     */
    pub fn seek(&mut self, key: &[u8]) {
        self.underlying.seek(key)
    }
}

impl Iterator<(Vec<u8>, Vec<u8>)> for DBIteratorAlloc {
    fn next(&mut self) -> Option<(Vec<u8>, Vec<u8>)> {
        match self.underlying.next() {
            Some((key, val)) => {
                Some((Vec::from_slice(key), Vec::from_slice(val)))
            },
            None => None,
        }
    }
}

/**
 * An immutable snapshot of the database at a point in time.
 */
pub struct DBSnapshot<'a> {
    sn: *mut cffi::leveldb_snapshot_t,

    // TODO: we need a mutable pointer to DB to drop a snapshot
    // Should check if we actually mutate things, and, if so, convert this to
    // a mutable reference.  For now, though, this is immutable so that we can
    // have multiple different snapshots.
    db: &'a DB,
}

impl<'a> DBSnapshot<'a> {
    // Note: deliberately not public
    fn new_from<'a>(db: &'a DB) -> DBSnapshot<'a> {
        let sn = unsafe { cffi::leveldb_create_snapshot(db.db) };

        DBSnapshot {
            sn: sn,
            db: db,
        }
    }
}

#[unsafe_destructor]
impl<'a> Drop for DBSnapshot<'a> {
    fn drop(&mut self) {
        // TODO: is this necessary?
        if self.sn.is_null() { return }

        unsafe {
            cffi::leveldb_release_snapshot(
                self.db.db,
                self.sn as *const cffi::leveldb_snapshot_t,
            )
        };

        self.sn = ptr::mut_null();
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

        self.put_opts(key, val, opts)
    }

    /**
     * Set the database entry for "key" to "value".  Allows specifying the
     * write options to use for this operaton.
     */
    pub fn put_opts(&mut self, key: &[u8], val: &[u8], opts: DBWriteOptions) -> LevelDBResult<()> {
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

    /**
     * Remove the database entry (if any) for "key".  Returns a result
     * indicating the success of the operation.  It is not an error if "key"
     * did not exist in the database.
     */
    pub fn delete(&mut self, key: &[u8]) -> LevelDBResult<()> {
        // TODO: proper return code for OOM
        let opts = match DBWriteOptions::new() {
            Some(o) => o,
            None    => fail!("Out of memory"),
        };

        self.delete_opts(key, opts)
    }

    /**
     * Remove the database entry (if any) for "key".  As `delete()`, but allows
     * specifying the write options to use for this operation.
     */
    pub fn delete_opts(&mut self, key: &[u8], opts: DBWriteOptions) -> LevelDBResult<()> {
        try!(with_errptr(|errptr| {
            unsafe {
                cffi::leveldb_delete(
                    self.db,
                    opts.ptr(),
                    key.as_ptr() as *const c_char,
                    key.len() as size_t,
                    errptr
                )
            }
        }))

        Ok(())
    }

    /**
     * Apply the specified updates to the database, as given in the provided
     * DBWriteBatch.  Returns a result indicating the success of the operation.
     */
    pub fn write(&mut self, batch: DBWriteBatch) -> LevelDBResult<()> {
        // TODO: proper return code for OOM
        let opts = match DBWriteOptions::new() {
            Some(o) => o,
            None    => fail!("Out of memory"),
        };

        self.write_opts(batch, opts)
    }

    /**
     * Apply the given write batch.  As `write()`, but allows specifying the
     * write options to use for this operation.
     */
    pub fn write_opts(&mut self, batch: DBWriteBatch, opts: DBWriteOptions) -> LevelDBResult<()> {
        try!(with_errptr(|errptr| {
            unsafe {
                cffi::leveldb_write(
                    self.db,
                    opts.ptr(),
                    batch.batch,
                    errptr
                )
            }
        }))
        Ok(())
    }

    /**
     * If the database contains an entry for "key", return the associated value
     * - otherwise, return None.  This value is wrapped in a Result to indicate
     * if an error occurred.
     */
    pub fn get(&mut self, key: &[u8]) -> LevelDBResult<Option<Vec<u8>>> {
        // TODO: proper return code for OOM
        let opts = match DBReadOptions::new() {
            Some(o) => o,
            None    => fail!("Out of memory"),
        };

        self.get_opts(key, opts)
    }

    /**
     * Get the value for a given key.  As `get()`, but allows specifying the
     * options to use when reading.
     */
    pub fn get_opts(&mut self, key: &[u8], opts: DBReadOptions) -> LevelDBResult<Option<Vec<u8>>> {
        let mut size: size_t = 0;

        let buff = try!(with_errptr(|errptr| {
            unsafe {
                cffi::leveldb_get(
                    self.db,
                    opts.ptr(),
                    key.as_ptr() as *const c_char,
                    key.len() as size_t,
                    &mut size as *mut size_t,
                    errptr
                )
            }
        }));

        if buff.is_null() {
            return Ok(None)
        }

        let size = size as uint;

        // TODO: should investigate whether we can avoid another copy
        // perhaps use std::c_vec::CVec?
        let mut ret = Vec::with_capacity(size);

        unsafe {
            std::ptr::copy_nonoverlapping_memory(
                ret.as_mut_ptr(),
                buff as *const u8,
                size
            );

            ret.set_len(size);
        }

        Ok(Some(ret))
    }

    /**
     * Return an iterator over the database.
     */
    pub fn iter(&mut self) -> DBIterator {
        // TODO: proper return code for OOM
        let opts = match DBReadOptions::new() {
            Some(o) => o,
            None    => fail!("Out of memory"),
        };

        let it = unsafe {
            cffi::leveldb_create_iterator(
                self.db,
                opts.ptr()
            )
        };

        DBIterator::new(it)
    }

    /**
     * Return a snapshot of the database.
     */
    pub fn snapshot(&mut self) -> DBSnapshot {
        DBSnapshot::new_from(self)
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

    use super::{DB, DBWriteBatch};
    use super::ffi::ffi;

    fn new_temp_db(name: &str) -> DB {
        let tdir = match TempDir::new(name) {
            Some(t) => t,
            None    => fail!("Error creating temp dir"),
        };

        match DB::create(tdir.path()) {
            Ok(db)   => db,
            Err(why) => fail!("Error creating DB: {}", why),
        }
    }

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
        let mut db = new_temp_db("put");

        match db.put(b"foo", b"bar") {
            Ok(_)    => {},
            Err(why) => fail!("Error putting into DB: {}", why),
        };
    }

    #[test]
    fn test_put_and_get() {
        let mut db = new_temp_db("put-and-get");

        match db.put(b"foo", b"bar") {
            Ok(_)    => {},
            Err(why) => fail!("Error putting into DB: {}", why),
        };

        match db.get(b"foo") {
            Ok(v)    => assert_eq!(v.expect("Value not found").as_slice(), b"bar"),
            Err(why) => fail!("Error getting from DB: {}", why),
        };
    }

    #[test]
    fn test_delete() {
        let mut db = new_temp_db("delete");

        db.put(b"foo", b"bar").unwrap();
        db.put(b"abc", b"123").unwrap();

        // Note: get --> unwrap Result --> expect Option --> convert Vec to slice
        assert_eq!(db.get(b"foo").unwrap().expect("Value not found").as_slice(), b"bar");
        assert_eq!(db.get(b"abc").unwrap().expect("Value not found").as_slice(), b"123");

        match db.delete(b"foo") {
            Ok(_)    => {},
            Err(why) => fail!("Error deleting from DB: {}", why),
        }

        assert_eq!(db.get(b"foo").unwrap(), None);
        assert_eq!(db.get(b"abc").unwrap().expect("Value not found").as_slice(), b"123");
    }

    #[test]
    fn test_write_batch() {
        let mut db = new_temp_db("write-batch");

        db.put(b"foo", b"bar").unwrap();
        db.put(b"abc", b"123").unwrap();

        let mut batch = DBWriteBatch::new().expect("Error creating batch");

        batch.put(b"def", b"456");
        batch.put(b"zzz", b"asdfgh");
        batch.delete(b"abc");
        batch.put(b"zzz", b"qwerty");

        match db.write(batch) {
            Ok(_)    => {},
            Err(why) => fail!("Error writing to DB: {}", why),
        };

        assert_eq!(db.get(b"foo").unwrap().expect("Value not found").as_slice(), b"bar");
        assert_eq!(db.get(b"def").unwrap().expect("Value not found").as_slice(), b"456");
        assert_eq!(db.get(b"zzz").unwrap().expect("Value not found").as_slice(), b"qwerty");
    }

    #[test]
    fn test_iteration() {
        let mut db = new_temp_db("iteration");

        db.put(b"foo", b"bar").unwrap();
        db.put(b"abc", b"123").unwrap();

        let mut it = db.iter();

        let t1 = match it.next() {
            Some((key, val)) => {
                (Vec::from_slice(key), Vec::from_slice(val))
            },
            None => fail!("Expected item 1"),
        };
        let t2 = match it.next() {
            Some((key, val)) => {
                (Vec::from_slice(key), Vec::from_slice(val))
            },
            None => fail!("Expected item 2"),
        };
        let t3 = it.next();

        // Keys are stored ordered, despite the fact we inserted unordered.
        assert_eq!(t1.ref0().as_slice(), b"abc");
        assert_eq!(t1.ref1().as_slice(), b"123");

        assert_eq!(t2.ref0().as_slice(), b"foo");
        assert_eq!(t2.ref1().as_slice(), b"bar");

        assert!(t3.is_none());
    }

    #[test]
    fn test_iteration_alloc() {
        let mut db = new_temp_db("iteration");

        db.put(b"foo", b"bar").unwrap();
        db.put(b"abc", b"123").unwrap();

        let items: Vec<(Vec<u8>, Vec<u8>)> = db.iter().alloc().collect();

        assert_eq!(items.len(), 2u);
        assert_eq!(items[0].ref0().as_slice(), b"abc");
        assert_eq!(items[0].ref1().as_slice(), b"123");
        assert_eq!(items[1].ref0().as_slice(), b"foo");
        assert_eq!(items[1].ref1().as_slice(), b"bar");
    }
}
