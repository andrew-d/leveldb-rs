use std::io::{Command, fs};
use std::os;
use std::path::Path;

// TODO: include this from a common location
const SNAPPY_VERSION: &'static str  = "1.1.2";

fn main() {
    println!("[build] Started");

    // Step 1: Build snappy
    // ----------------------------------------------------------------------
    let snappy_path = Path::new("..")
                           .join("deps")
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

    // Step 2: Copy to output directories
    // ----------------------------------------------------------------------
    let out_dir = Path::new(os::getenv("OUT_DIR").unwrap());

    println!("[build] Copying output files");
    fs::copy(&snappy_path.join(".libs").join("libsnappy.a"), &out_dir.join("libsnappy.a")).unwrap();

    // Step 3: Tell Rust about what we link to
    // ----------------------------------------------------------------------
    let out_dir = os::getenv("OUT_DIR").unwrap();
    println!("cargo:rustc-flags=-L {} -l snappy:static", out_dir);
    println!("[build] Finished");
}
