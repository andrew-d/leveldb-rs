use std::io::{Command, fs};
use std::os;
use std::path::Path;

const SNAPPY_VERSION: &'static str  = "1.1.2";
const LEVELDB_VERSION: &'static str = "1.15.0";


fn main() {
    println!("[build] Started");

    // Step 1: Build snappy
    // ----------------------------------------------------------------------
    let snappy_path = Path::new("deps")
                           .join(format!("snappy-{}", SNAPPY_VERSION));

    // Clean the build directory first.
    println!("[snappy] Cleaning");
    Command::new("make").args(&["-C", snappy_path.as_str().unwrap()])
                        .arg("distclean")
                        .status().unwrap();

    // Configure the build
    println!("[snappy] Configuring");
    Command::new("./configure").cwd(&snappy_path)
                               .arg("CXXFLAGS=-fPIC")
                               .status().unwrap();

    // Call "make" to build the C library
    println!("[snappy] Building");
    Command::new("make").args(&["-C", snappy_path.as_str().unwrap()])
                        .status().unwrap();

    // Step 2: Build leveldb
    // ----------------------------------------------------------------------
    let leveldb_path = Path::new("deps")
                            .join(format!("leveldb-{}", LEVELDB_VERSION));

    // Clean the build directory first.
    println!("[leveldb] Cleaning");
    Command::new("make").args(&["-C", leveldb_path.as_str().unwrap()])
                        .arg("clean")
                        .status().unwrap();

    // Make the library
    println!("[leveldb] Building");
    Command::new("make").args(&["-C", leveldb_path.as_str().unwrap()])
                        .env("LDFLAGS",  format!("-L{}",       snappy_path.as_str().unwrap()))
                        .env("CXXFLAGS", format!("-I{} -fPIC", snappy_path.as_str().unwrap()))
                        .status().unwrap();

    // Step 3: Copy to output directories
    // ----------------------------------------------------------------------
    let out_dir = Path::new(os::getenv("OUT_DIR").unwrap());

    println!("[build] Copying output files");
    fs::copy(&snappy_path.join(".libs").join("libsnappy.a"), &out_dir.join("libsnappy.a")).unwrap();
    fs::copy(&leveldb_path.join("libleveldb.a"), &out_dir.join("libleveldb.a")).unwrap();

    // Step 4: Tell Rust about what we link to
    // ----------------------------------------------------------------------
    let out_dir = os::getenv("OUT_DIR").unwrap();
    println!("cargo:rustc-flags=-L {} -l snappy:static -l leveldb:static", out_dir);
    println!("[build] Finished");
}
