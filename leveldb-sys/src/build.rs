use std::env;
use std::io::{BufReader, BufRead, Write};
use std::path::PathBuf;
use std::process::Command;
use std::fs::{self, File};
use std::os::unix::fs::OpenOptionsExt;
use std::path::Path;


const SNAPPY_VERSION: &'static str  = "1.1.2";
const LEVELDB_VERSION: &'static str = "1.18";


fn build_snappy() {
    // Step 1: Build snappy
    // ----------------------------------------------------------------------
    let snappy_path = Path::new("deps")
                           .join(format!("snappy-{}", SNAPPY_VERSION));

    // Clean the build directory first.
    println!("[snappy] Cleaning");
    Command::new("make").arg("-C")
                        .arg(&snappy_path)
                        .arg("distclean")
                        .status().unwrap();

    // Configure the build
    println!("[snappy] Configuring");
    Command::new("./configure").current_dir(&snappy_path)
                               .arg("CXXFLAGS=-fPIC")
                               .status().unwrap();

    // Call "make" to build the C library
    println!("[snappy] Building");
    Command::new("make").arg("-C")
                        .arg(&snappy_path)
                        .status().unwrap();

    // Step 2: Copy to output directories
    // ----------------------------------------------------------------------
    let mut out_dir = PathBuf::new();
    out_dir.push(env::var("OUT_DIR").unwrap());

    println!("[build] Copying output files");
    fs::copy(&snappy_path.join(".libs").join("libsnappy.a"), &out_dir.join("libsnappy.a")).unwrap();
}

fn build_leveldb(with_snappy: bool) {
    // Step 1: Build LevelDB
    // ----------------------------------------------------------------------
    let leveldb_path = Path::new("deps")
                            .join(format!("leveldb-{}", LEVELDB_VERSION));

    // Clean the build directory first.
    println!("[leveldb] Cleaning");
    Command::new("make").arg("-C")
                        .arg(&leveldb_path)
                        .arg("clean")
                        .status().unwrap();

    // Our command builder
    let mut cmd = Command::new("make");
    let mut pbuild: &mut Command = &mut cmd
            .arg("-C")
            .arg(&leveldb_path);

    // Set up the process environment.  We essentially clone the existing
    // environment, and, if we're including Snappy, also include the appropriate
    // CXXFLAGS and LDFLAGS variables.
    for (key, val) in env::vars() {
        // TODO: this is ugly :'(
        let mut tmp = pbuild;
        let tmp2 = tmp.env(key, val);
        pbuild = tmp2;
    }

    if with_snappy {
        {
            // TODO: this is ugly :'(
            let linker_path = env::var("OUT_DIR").unwrap();
            let mut tmp = pbuild;
            let tmp2 = tmp.env("LDFLAGS", format!("-L{}", linker_path));
            pbuild = tmp2;
        }

        {
            // TODO: this is ugly :'(
            let snappy_path = Path::new("deps")
                                   .join(format!("snappy-{}", SNAPPY_VERSION));
            let mut tmp = pbuild;
            let tmp2 = tmp.env("CXXFLAGS", format!("-I{} -fPIC", snappy_path.to_string_lossy()));
            pbuild = tmp2;
        }
    } else {
        // TODO: this is ugly :'(
        let mut tmp = pbuild;
        let tmp2 = tmp.env("CXXFLAGS", "-fPIC");
        pbuild = tmp2;
    }

    // Build the library
    println!("[leveldb] Building");
    let stat = pbuild.status().unwrap_or_else(|e| {
        panic!("failed to execute process: {}", e)
    });
    if !stat.success() {
        panic!("[leveldb] process exited with non-zero status: {}", stat);
    }
    println!("[leveldb] Build complete: {}", stat);

    // Step 2: Copy to output directories
    // ----------------------------------------------------------------------
    let mut out_dir = PathBuf::new();
    out_dir.push(env::var("OUT_DIR").unwrap());

    println!("[build] Copying output files");
    fs::copy(&leveldb_path.join("libleveldb.a"), &out_dir.join("libleveldb.a")).unwrap();
}

fn main() {
    println!("[build] Started");

    let have_snappy = env::var_os("CARGO_FEATURE_SNAPPY").is_some();

    // If we have the appropriate feature, then we build snappy.
    if have_snappy {
        build_snappy();
    }

    // Copy the build_detect_platform file into the appropriate place.
    let template_path = Path::new("deps")
                             .join("build_detect_platform");
    let detect_path = Path::new("deps")
                           .join(format!("leveldb-{}", LEVELDB_VERSION))
                           .join("build_detect_platform");
    if have_snappy {
        println!("[build] Copying the `build_detect_platform` template");
        fs::copy(&template_path, &detect_path).unwrap();
    } else {
        println!("[build] Patching the `build_detect_platform` template");

        // If we aren't using snappy, remove the lines from
        // build_detect_platform that enable Snappy.  This prevents us from
        // picking up a system-local copy of Snappy.
        let new_lines: Vec<String> = {
            let file = BufReader::new(File::open(&template_path).unwrap());

            file.lines().map(|line| {
                let line = line.unwrap();
                if line.contains("-DSNAPPY") || line.contains("-lsnappy") {
                    let mut tmp = String::new();
                    tmp.push_str("true   #");
                    tmp.push_str(&*line);
                    tmp
                } else {
                    line
                }
            }).collect()
        };

        let mut f = fs::OpenOptions::new()
                        .create(true)
                        .write(true)
                        .mode(0o755)
                        .open(&detect_path)
                        .unwrap();
        for line in new_lines.iter() {
            write!(f, "{}\n", line).unwrap();
        }

        println!("[build] Patching complete");
    }

    // Build LevelDB
    build_leveldb(have_snappy);

    // Print the appropriate linker flags
    let out_dir = env::var("OUT_DIR").unwrap();
    let linker_flags = if have_snappy {
        "-l static=snappy -l static=leveldb -l stdc++"
    } else {
        "-l static=leveldb -l stdc++"
    };
    println!("cargo:rustc-flags=-L native={} {} -l stdc++", out_dir, linker_flags);

    println!("[build] Finished");
}
