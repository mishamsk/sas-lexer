extern crate rustc_version;

use rustc_version::{version_meta, Channel};

fn main() {
    println!("cargo:rustc-check-cfg=cfg(rustc_nightly)");

    let version_meta = version_meta().expect("No rustc version found!");
    if version_meta.channel == Channel::Nightly {
        println!("cargo:rustc-cfg=rustc_nightly");
    }
}
