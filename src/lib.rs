/*!
 * Rust bindings for [LevelDB](https://code.google.com/p/leveldb/), a fast and
 * lightweight key/value database library from Google.
 *
 * Warning: Some portions of this library are still unsafe to use, in that it
 * is possible to call methods from LevelDB with stale pointers, or otherwise
 * cause memory-unsafety.  If you'd like to avoid this, and until I fix them,
 * please don't use:
 *
 * - Custom comparators
 * - DB snapshots
 *
 * And please be careful with write batches.  Patches are welcome!
 */
#![crate_name = "leveldb"]
#![comment = "Bindings to LevelDB"]
#![license = "MIT"]
#![crate_type = "lib"]
#![warn(missing_docs)]
#![warn(non_upper_case_globals)]
#![warn(unused_qualifications)]
#![feature(globs)]
#![feature(unsafe_destructor)]

extern crate libc;

use std::ptr;
use std::raw::Slice;
use std::rc::Rc;
use std::mem::transmute;

use libc::{c_char, c_int, c_uchar, c_void};
use libc::types::os::arch::c95::size_t;

use ffi::ffi as cffi;

mod ffi;


/// Our error type
#[deriving(Clone, Hash, PartialEq, Eq)]
pub enum LevelDBError {
    /// An error from the LevelDB C library.
    LibraryError(String),

    /// Out of memory.
    OutOfMemoryError,
}

impl std::fmt::Show for LevelDBError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::FormatError> {
        match *self {
            LibraryError(ref msg) => msg.fmt(f),
            OutOfMemoryError      => write!(f, "Out of memory"),
        }
    }
}

impl LevelDBError {
    fn lib_error(errptr: *mut c_char) -> LevelDBError {
        // Convert to a rust String, then free the LevelDB string.
        let err = unsafe {
            std::string::raw::from_buf(errptr as *const u8)
        };
        unsafe { cffi::leveldb_free(errptr as *mut c_void) };

        LibraryError(err)
    }
}

pub type LevelDBResult<T> = Result<T, LevelDBError>;

// Convert a Path instance to a C-style string
fn path_as_c_str<T>(path: &Path, f: |*const i8| -> T) -> T {
    // First, convert the path to a vector...
    let mut pvec = path.as_vec().to_vec();

    // ... and ensure that it's null-terminated.
    if pvec[pvec.len() - 1] != 0 {
        pvec.push(0);
    }

    // Now, call the function with the new path pointer.
    // This also returns what the function does.
    f(pvec.as_ptr() as *const i8)
}

// Provides an errptr for use with LevelDB, and properly returns a Result if
// it's non-null.
fn with_errptr<T>(f: |*mut *mut c_char| -> T) -> LevelDBResult<T> {
    let mut errptr: *mut c_char = ptr::null_mut();

    let ret = f(&mut errptr as *mut *mut c_char);

    if !errptr.is_null() {
        Err(LevelDBError::lib_error(errptr))
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

    // An (optional) comparator.  We hold on to a pointer to this to keep it
    // alive for the lifetime of this options struct.
    comparator: Option<DBComparator>,
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
                comparator: None,
            })
        }
    }

    /**
     * Set the comparator to use for this database.  By default, LevelDB uses
     * a comparator does a lexicographic comparison.
     * Note also that a comparator must be thread-safe.
     */
    pub fn set_comparator(&mut self, cmp: DBComparator) -> &mut DBOptions {
        unsafe {
            cffi::leveldb_options_set_comparator(self.opts, cmp.state.ptr);
        }

        // Hold on to this comparator so it gets dropped (and thus destroyed/
        // freed) when we do.
        self.comparator = Some(cmp);

        self
    }

    /**
     * Create the database if it's missing when we try to open it.
     *
     * Default: false
     */
    pub fn set_create_if_missing(&mut self, val: bool) -> &mut DBOptions {
        unsafe {
            cffi::leveldb_options_set_create_if_missing(self.opts, bool_to_uchar(val));
        }

        self
    }

    /**
     * Return an error if the database already exists.
     *
     * Default: false
     */
    pub fn set_error_if_exists(&mut self, val: bool) -> &mut DBOptions {
        unsafe {
            cffi::leveldb_options_set_error_if_exists(self.opts, bool_to_uchar(val));
        }

        self
    }

    /**
     * If set to true, the library will do aggressive checking of all data
     * that it is processing and will stop early if it detects any errors.
     *
     * Default: false
     */
    pub fn set_paranoid_checks(&mut self, val: bool) -> &mut DBOptions {
        unsafe {
            cffi::leveldb_options_set_paranoid_checks(self.opts, bool_to_uchar(val));
        }

        self
    }

    /**
     * Amount of data to build up in memory (backed by an unsorted log on-disk)
     * before converting to a sorted on-disk file.
     *
     * Default: 4MiB
     */
    pub fn set_write_buffer_size(&mut self, val: uint) -> &mut DBOptions {
        unsafe {
            cffi::leveldb_options_set_write_buffer_size(self.opts, val as size_t);
        }

        self
    }

    /**
     * Number of open files that can be used by the DB.  This value should be
     * approximately one open file per 2MB of working set.
     *
     * Default: 1000
     */
    pub fn set_max_open_files(&mut self, val: int) -> &mut DBOptions {
        unsafe {
            cffi::leveldb_options_set_max_open_files(self.opts, val as c_int);
        }

        self
    }

    /**
     * Approximate size of user data packed per block.  Note that this
     * corresponds to uncompressed data.
     *
     * Default: 4KB
     */
    pub fn set_block_size(&mut self, val: uint) -> &mut DBOptions {
        unsafe {
            cffi::leveldb_options_set_block_size(self.opts, val as size_t);
        }

        self
    }

    /**
     * Number of keys between restart points for delta encoding of keys.  Most
     * clients should not change this parameter.
     *
     * Default: 16
     */
    pub fn set_block_restart_interval(&mut self, val: int) -> &mut DBOptions {
        unsafe {
            cffi::leveldb_options_set_block_restart_interval(self.opts, val as c_int);
        }

        self
    }

    /**
     * Enable or disable compression.  Note that the default compression
     * algorithm, Snappy, is significantly faster than most persistent storage.
     * Thus, it's typically never worth switching this off.
     *
     * Default: true
     */
    pub fn set_compression(&mut self, val: bool) -> &mut DBOptions {
        let val = if val {
            cffi::LEVELDB_SNAPPY_COMPRESSION
        } else {
            cffi::LEVELDB_NO_COMPRESSION
        };

        unsafe {
            cffi::leveldb_options_set_compression(self.opts, val);
        }

        self
    }

    unsafe fn ptr(&self) -> *const cffi::leveldb_options_t {
        self.opts as *const cffi::leveldb_options_t
    }
}

#[unsafe_destructor]
impl Drop for DBOptions {
    fn drop(&mut self) {
        unsafe { cffi::leveldb_options_destroy(self.opts) };
    }
}

/**
 * An internal structure to represent the comparator state.  Note that, since
 * the comparator struct is movable, we can't take the address of it and pass
 * that to leveldb_comparator_create.  So, we keep the internal state in a Box,
 * and can move the outer struct around freely.
 */
struct DBComparatorState {
    name: &'static str,
    cmp: |&[u8], &[u8]|: 'static -> Ordering,
    ptr: *mut cffi::leveldb_comparator_t,
}

#[unsafe_destructor]
impl Drop for DBComparatorState {
    fn drop(&mut self) {
        unsafe { cffi::leveldb_comparator_destroy(self.ptr) };
    }
}

/**
 * This structure represents a comparator for use in LevelDB.
 */
pub struct DBComparator {
    state: Box<DBComparatorState>,
}

impl DBComparator {
    /**
     * Create a new comparator with the given name and comparison function.
     */
    pub fn new(name: &'static str, cmp: |&[u8], &[u8]|: 'static -> Ordering) -> DBComparator {
        let mut state = box DBComparatorState {
            name: name,
            cmp: cmp,
            ptr: ptr::null_mut(),
        };

        let ptr = unsafe {
            cffi::leveldb_comparator_create(
                transmute(&*state),
                comparator_destructor_callback,
                comparator_compare_callback,
                comparator_name_callback,
            )
        };

        state.ptr = ptr;

        DBComparator {
            state: state,
        }
    }
}

#[allow(dead_code)]
extern "C" fn comparator_destructor_callback(_state: *mut c_void) {
    // Do nothing
}

#[allow(dead_code)]
extern "C" fn comparator_compare_callback(state: *mut c_void, a: *const c_char, alen: size_t, b: *const c_char, blen: size_t) -> c_int {
    unsafe {
        // This is only safe since Box<T> is implemented as `struct Box(*mut T)`
        let cmp: *const DBComparatorState = transmute(state);

        let a_slice = transmute(Slice {
            data: a,
            len:  alen as uint,
        });

        let b_slice = transmute(Slice {
            data: b,
            len:  blen as uint,
        });

        // Comment from include/leveldb/comparator.h:
        // Three-way comparison.  Returns value:
        //   < 0 iff "a" < "b",
        //   == 0 iff "a" == "b",
        //   > 0 iff "a" > "b"
        match ((*cmp).cmp)(a_slice, b_slice) {
            Less => -1,
            Equal => 0,
            Greater => 1,
        }
    }
}

#[allow(dead_code)]
extern "C" fn comparator_name_callback(state: *mut c_void) -> *const c_char {
    unsafe {
        // This is only safe since Box<T> is implemented as `struct Box(*mut T)`
        let cmp: *const DBComparatorState = transmute(state);

        // This is safe to return, since the string has a static lifetime
        (*cmp).name.as_ptr() as *const c_char
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
     *
     * Note: currently private, since all access should be performed through
     * DBSnapshot
     */
    fn set_snapshot(&mut self, snap: *const cffi::leveldb_snapshot_t) -> &mut DBReadOptions {
        unsafe {
            cffi::leveldb_readoptions_set_snapshot(self.opts, snap);
        }

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

        self
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

    /**
     * Iterate over the contents of the write batch by calling callbacks for
     * each operation in the batch.
     */
    pub fn iterate<'a>(&'a self, put: |&'a [u8], &'a [u8]|: 'a, delete: |&'a [u8]|: 'a) {
        let mut it = DBWriteBatchIter {
            put: put,
            delete: delete,
        };

        unsafe {
            cffi::leveldb_writebatch_iterate(
                self.batch,
                &mut it as *mut _ as *mut c_void,
                writebatch_put_callback,
                writebatch_delete_callback
            );
        };
    }
}

struct DBWriteBatchIter<'a> {
    pub put:    |&'a [u8], &'a [u8]|: 'a,
    pub delete: |&'a [u8]|: 'a,
}

// Callback for DBWriteBatchIter
extern "C" fn writebatch_put_callback(state: *mut c_void, key: *const c_char, klen: size_t, val: *const c_char, vlen: size_t) {
    let it = state as *mut DBWriteBatchIter;

    let key_slice = unsafe {
        transmute(Slice {
            data: key,
            len:  klen as uint,
        })
    };

    let val_slice = unsafe {
        transmute(Slice {
            data: val,
            len:  vlen as uint,
        })
    };

    unsafe { ((*it).put)(key_slice, val_slice) };
}

// Callback for DBWriteBatchIter
extern "C" fn writebatch_delete_callback(state: *mut c_void, key: *const c_char, klen: size_t) {
    let it = state as *mut DBWriteBatchIter;

    let key_slice = unsafe {
        transmute(Slice {
            data: key,
            len:  klen as uint,
        })
    };

    unsafe { ((*it).delete)(key_slice) };
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

    /**
     * Move to the previous item in the database.
     */
    pub fn prev(&mut self) {
        unsafe { cffi::leveldb_iter_prev(self.iter) };
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
                Some((key.to_vec(), val.to_vec()))
            },
            None => None,
        }
    }
}

/**
 * An immutable snapshot of the database at a point in time.
 */
pub struct DBSnapshot {
    sn: *mut cffi::leveldb_snapshot_t,

    // We can't save a pointer to the underlying DB itself, since that would
    // prevent any further mutation (something that we want to allow).
    db: DBImplPtr,
}

impl DBSnapshot {
    // Note: deliberately not public
    fn new_from(db: &DBImplPtr) -> DBSnapshot {
        // Clone the underlying database to ensure that it doesn't go away.
        let db = db.clone();

        let sn = unsafe { cffi::leveldb_create_snapshot(db.db) };

        DBSnapshot {
            sn: sn,
            db: db,
        }
    }

    /**
     * As `DB.get`, except operating on the state of this snapshot.
     */
    pub fn get(&self, key: &[u8]) -> LevelDBResult<Option<Vec<u8>>> {
        // TODO: proper return code for OOM
        let opts = match DBReadOptions::new() {
            Some(o) => o,
            None    => return Err(OutOfMemoryError),
        };

        self.get_opts(key, opts)
    }

    /**
     * As `DB.get_opts`, except operating on the state of this snapshot.
     */
    pub fn get_opts(&self, key: &[u8], opts: DBReadOptions) -> LevelDBResult<Option<Vec<u8>>> {
        let mut opts = opts;

        opts.set_snapshot(self.sn as *const cffi::leveldb_snapshot_t);
        self.db.get(key, opts)
    }

    /**
     * As `DB.iter`, except operating on the state of this snapshot.
     */
    pub fn iter(&self) -> LevelDBResult<DBIterator> {
        // TODO: proper return code for OOM
        let mut opts = match DBReadOptions::new() {
            Some(o) => o,
            None    => return Err(OutOfMemoryError),
        };

        opts.set_snapshot(self.sn as *const cffi::leveldb_snapshot_t);
        Ok(self.db.iter(opts))
    }

}

#[unsafe_destructor]
impl Drop for DBSnapshot {
    fn drop(&mut self) {
        unsafe {
            cffi::leveldb_release_snapshot(
                self.db.db,
                self.sn as *const cffi::leveldb_snapshot_t,
            )
        };
    }
}

/*
 * This internal structure keeps a reference to the underlying database, along
 * with any other information that needs to be freed when the database goes out
 * of scope (e.g. DB options).  It also provides the base interfaces for
 * reading/writing to the database.
 *
 * WARNING: The methods on this take &self, since LevelDB itself is safe for
 * concurrent access without any synchronization.  Note that this *does* mutate
 * the database!  We enforce synchronization at the DB / DBSnapshot level, as
 * opposed to this level.
 */
struct DBImpl {
    // The DB handle
    db: *mut cffi::leveldb_t,

    // The options used to open the database.  We need to keep this alive for
    // the lifetime of the database.
    #[allow(dead_code)]
    opts: DBOptions,
}

impl DBImpl {
    fn open(path: &Path, opts: DBOptions) -> LevelDBResult<DBImplPtr> {
        let res = path_as_c_str(path, |path| {
            with_errptr(|errptr| {
                unsafe { cffi::leveldb_open(opts.ptr(), path, errptr) }
            })
        });

        let db = match res {
            Ok(db) => db,
            Err(v) => return Err(v),
        };

        Ok(Rc::new(DBImpl {
            db: db,
            opts: opts,
        }))
    }

    fn put(&self, key: &[u8], val: &[u8], opts: DBWriteOptions) -> LevelDBResult<()> {
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

    fn delete(&self, key: &[u8], opts: DBWriteOptions) -> LevelDBResult<()> {
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

    fn write(&self, batch: DBWriteBatch, opts: DBWriteOptions) -> LevelDBResult<()> {
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

    fn get(&self, key: &[u8], opts: DBReadOptions) -> LevelDBResult<Option<Vec<u8>>> {
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

            cffi::leveldb_free(buff as *mut c_void);
        }

        Ok(Some(ret))
    }

    fn iter(&self, opts: DBReadOptions) -> DBIterator {
        let it = unsafe {
            cffi::leveldb_create_iterator(
                self.db,
                opts.ptr()
            )
        };

        DBIterator::new(it)
    }
}

#[unsafe_destructor]
impl Drop for DBImpl {
    fn drop(&mut self) {
        unsafe {
            cffi::leveldb_close(self.db)
        }
    }
}

// A reference-counted pointer to a database implementation.  We need to use
// this, since snapshots can hold on to a reference to a DB.
type DBImplPtr = Rc<DBImpl>;

/**
 * This struct represents an open instance of the database.
 */
pub struct DB {
    db: DBImplPtr,
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
            None    => return Err(OutOfMemoryError),
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
            None    => return Err(OutOfMemoryError),
        };

        // TODO: can we remove a previously-existing database?

        opts.set_create_if_missing(true);
        DB::open_with_opts(path, opts)
    }

    /**
     * Open a database at the given path, using the provided options to control
     * the open behaviour.  Returns a Result indicating whether or not the
     * database could be opened.
     */
    pub fn open_with_opts(path: &Path, opts: DBOptions) -> LevelDBResult<DB> {
        match DBImpl::open(path, opts) {
            Ok(x)    => Ok(DB { db: x }),
            Err(why) => Err(why),
        }
    }

    /**
     * Set the database entry for "key" to "value". Returns a result indicating
     * the success or failure of the operation.
     */
    pub fn put(&mut self, key: &[u8], val: &[u8]) -> LevelDBResult<()> {
        // TODO: proper return code for OOM
        let opts = match DBWriteOptions::new() {
            Some(o) => o,
            None    => return Err(OutOfMemoryError),
        };

        self.put_opts(key, val, opts)
    }

    /**
     * Set the database entry for "key" to "value".  Allows specifying the
     * write options to use for this operaton.
     */
    pub fn put_opts(&mut self, key: &[u8], val: &[u8], opts: DBWriteOptions) -> LevelDBResult<()> {
        self.db.put(key, val, opts)
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
            None    => return Err(OutOfMemoryError),
        };

        self.delete_opts(key, opts)
    }

    /**
     * Remove the database entry (if any) for "key".  As `delete()`, but allows
     * specifying the write options to use for this operation.
     */
    pub fn delete_opts(&mut self, key: &[u8], opts: DBWriteOptions) -> LevelDBResult<()> {
        self.db.delete(key, opts)
    }

    /**
     * Apply the specified updates to the database, as given in the provided
     * DBWriteBatch.  Returns a result indicating the success of the operation.
     */
    pub fn write(&mut self, batch: DBWriteBatch) -> LevelDBResult<()> {
        // TODO: proper return code for OOM
        let opts = match DBWriteOptions::new() {
            Some(o) => o,
            None    => return Err(OutOfMemoryError),
        };

        self.write_opts(batch, opts)
    }

    /**
     * Apply the given write batch.  As `write()`, but allows specifying the
     * write options to use for this operation.
     */
    pub fn write_opts(&mut self, batch: DBWriteBatch, opts: DBWriteOptions) -> LevelDBResult<()> {
        self.db.write(batch, opts)
    }

    /**
     * If the database contains an entry for "key", return the associated value
     * - otherwise, return None.  This value is wrapped in a Result to indicate
     * if an error occurred.
     */
    pub fn get(&self, key: &[u8]) -> LevelDBResult<Option<Vec<u8>>> {
        // TODO: proper return code for OOM
        let opts = match DBReadOptions::new() {
            Some(o) => o,
            None    => return Err(OutOfMemoryError),
        };

        self.get_opts(key, opts)
    }

    /**
     * Get the value for a given key.  As `get()`, but allows specifying the
     * options to use when reading.
     */
    pub fn get_opts(&self, key: &[u8], opts: DBReadOptions) -> LevelDBResult<Option<Vec<u8>>> {
        self.db.get(key, opts)
    }

    /**
     * Return an iterator over the database.
     */
    pub fn iter(&mut self) -> LevelDBResult<DBIterator> {
        // TODO: proper return code for OOM
        let opts = match DBReadOptions::new() {
            Some(o) => o,
            None    => return Err(OutOfMemoryError),
        };

        Ok(self.db.iter(opts))
    }

    /**
     * Return a snapshot of the database.
     */
    pub fn snapshot(&self) -> DBSnapshot {
        DBSnapshot::new_from(&self.db)
    }

    // TODO:
    //  - static `destroy()` and `repair`
    //  - set caching
    //  - approximate size / compact range
    //  - property values
    //  - filter policy (what's it do?)
    //  - solve various memory leaks / lifetime issues
}

#[cfg(test)]
mod tests {
    #![allow(unused_imports)]

    extern crate test;

    use std::io::TempDir;

    use super::{DB, DBComparator, DBOptions, DBReadOptions, DBWriteBatch};
    use super::ffi::ffi;

    fn new_temp_db(name: &str) -> DB {
        let tdir = match TempDir::new(name) {
            Ok(t)    => t,
            Err(why) => panic!("Error creating temp dir: {}", why),
        };

        match DB::create(tdir.path()) {
            Ok(db)   => db,
            Err(why) => panic!("Error creating DB: {}", why),
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
            Ok(t)    => t,
            Err(why) => panic!("Error creating temp dir: {}", why),
        };

        let _db = match DB::create(tdir.path()) {
            Ok(db)   => db,
            Err(why) => panic!("Error creating DB: {}", why),
        };
    }

    #[test]
    fn test_put() {
        let mut db = new_temp_db("put");

        match db.put(b"foo", b"bar") {
            Ok(_)    => {},
            Err(why) => panic!("Error putting into DB: {}", why),
        };
    }

    #[test]
    fn test_put_and_get() {
        let mut db = new_temp_db("put-and-get");

        match db.put(b"foo", b"bar") {
            Ok(_)    => {},
            Err(why) => panic!("Error putting into DB: {}", why),
        };

        match db.get(b"foo") {
            Ok(v)    => assert_eq!(v.expect("Value not found").as_slice(), b"bar"),
            Err(why) => panic!("Error getting from DB: {}", why),
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
            Err(why) => panic!("Error deleting from DB: {}", why),
        }

        assert_eq!(db.get(b"foo").unwrap(), None);
        assert_eq!(db.get(b"abc").unwrap().expect("Value not found").as_slice(), b"123");
    }

    #[test]
    fn test_write_batch() {
        let mut db = new_temp_db("write-batch");

        db.put(b"foo", b"bar").unwrap();
        db.put(b"abc", b"123").unwrap();

        // Test putting into a write batch

        let mut batch = DBWriteBatch::new().expect("Error creating batch");

        batch.put(b"def", b"456");
        batch.put(b"zzz", b"asdfgh");
        batch.delete(b"abc");
        batch.put(b"zzz", b"qwerty");

        // Test iteration

        let mut puts: Vec<(Vec<u8>, Vec<u8>)> = vec![];
        let mut deletes: Vec<Vec<u8>> = vec![];

        batch.iterate(|k, v| {
            puts.push((k.to_vec(), v.to_vec()));
        }, |k| {
            deletes.push(k.to_vec());
        });

        assert_eq!(puts.len(), 3);
        assert_eq!(deletes.len(), 1);

        // Test writing

        match db.write(batch) {
            Ok(_)    => {},
            Err(why) => panic!("Error writing to DB: {}", why),
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

        let mut it = db.iter().unwrap();

        let t1 = match it.next() {
            Some((key, val)) => {
                (key.to_vec(), val.to_vec())
            },
            None => panic!("Expected item 1"),
        };
        let t2 = match it.next() {
            Some((key, val)) => {
                (key.to_vec(), val.to_vec())
            },
            None => panic!("Expected item 2"),
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

        let items: Vec<(Vec<u8>, Vec<u8>)> = db.iter().unwrap().alloc().collect();

        assert_eq!(items.len(), 2u);
        assert_eq!(items[0].ref0().as_slice(), b"abc");
        assert_eq!(items[0].ref1().as_slice(), b"123");
        assert_eq!(items[1].ref0().as_slice(), b"foo");
        assert_eq!(items[1].ref1().as_slice(), b"bar");
    }

    #[test]
    fn test_comparator_create() {
        let _c = DBComparator::new("comparator-create", |a, b| {
            a.cmp(&b)
        });
    }

    #[test]
    fn test_comparator() {
        let c = DBComparator::new("foo", |a, b| {
            // Compare inverse
            b.cmp(&a)
        });

        let mut opts = DBOptions::new().expect("error creating options");
        opts.set_comparator(c).set_create_if_missing(true);

        let tdir = match TempDir::new("comparator") {
            Ok(t)    => t,
            Err(why) => panic!("Error creating temp dir: {}", why),
        };

        let mut db = match DB::open_with_opts(tdir.path(), opts) {
            Ok(db)   => db,
            Err(why) => panic!("Error creating DB: {}", why),
        };

        // Insert into the DB some values.
        db.put(b"aaaa", b"foo").unwrap();
        db.put(b"zzzz", b"bar").unwrap();

        // Extract the values as an ordered vector.
        let items: Vec<(Vec<u8>, Vec<u8>)> = db.iter().unwrap().alloc().collect();

        // Values should be in reverse order.
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].ref0().as_slice(), b"zzzz");
        assert_eq!(items[0].ref1().as_slice(), b"bar");
        assert_eq!(items[1].ref0().as_slice(), b"aaaa");
        assert_eq!(items[1].ref1().as_slice(), b"foo");
    }

    #[test]
    fn test_snapshot() {
        let mut db = new_temp_db("snapshot");

        db.put(b"foo", b"bar").unwrap();
        db.put(b"abc", b"123").unwrap();

        let snap = db.snapshot();

        db.put(b"abc", b"456").unwrap();

        let snap_val = match snap.get(b"abc") {
            Ok(val) => val.expect("Expected to find key 'abc'"),
            Err(why) => panic!("Error getting from DB: {}", why),
        };
        assert!(snap_val.as_slice() == b"123");

        let val = match db.get(b"abc") {
            Ok(val) => val.expect("Expected to find key 'abc'"),
            Err(why) => panic!("Error getting from DB: {}", why),
        };
        assert!(val.as_slice() == b"456");

        let iter_items: Vec<(Vec<u8>, Vec<u8>)> = snap.iter().unwrap().alloc().collect();
        let db_items: Vec<(Vec<u8>, Vec<u8>)> = db.iter().unwrap().alloc().collect();

        assert_eq!(iter_items.len(), 2);
        assert_eq!(db_items.len(), 2);

        assert_eq!(iter_items[0].ref0().as_slice(), b"abc");
        assert_eq!(iter_items[0].ref1().as_slice(), b"123");
        assert_eq!(iter_items[1].ref0().as_slice(), b"foo");
        assert_eq!(iter_items[1].ref1().as_slice(), b"bar");

        assert_eq!(db_items[0].ref0().as_slice(), b"abc");
        assert_eq!(db_items[0].ref1().as_slice(), b"456");
        assert_eq!(db_items[1].ref0().as_slice(), b"foo");
        assert_eq!(db_items[1].ref1().as_slice(), b"bar");
    }
}
