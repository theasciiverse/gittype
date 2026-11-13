//! Whether the system libraries a title lists in `system_deps` are present,
//! so the detail pane only names the ones that are missing. The names are
//! Debian package names (`libasound2`); Debian names a library's package
//! after its soname, so `av` looks for `libasound.so.2` in the dynamic
//! loader's cache, which also covers distributions that call the package
//! something else (Ubuntu's `libasound2t64`, Fedora's `alsa-lib`).

use std::collections::HashSet;
use std::sync::OnceLock;

/// The packages in `deps` whose library this machine lacks. A name av
/// cannot turn into a library is kept, since it can't be ruled out. Off
/// Linux nothing is listed: the names are Debian's.
pub fn missing(deps: &[String]) -> Vec<String> {
    if !cfg!(target_os = "linux") {
        return Vec::new();
    }
    let Some(have) = loader_cache() else {
        return deps.to_vec();
    };
    deps.iter().filter(|d| soname(d).is_none_or(|s| !have.contains(&s))).cloned().collect()
}

/// `libasound2` to `libasound.so.2`, `libstdc++6` to `libstdc++.so.6`;
/// None when the name doesn't follow that pattern.
fn soname(package: &str) -> Option<String> {
    let p = package.strip_suffix("t64").unwrap_or(package);
    if p == "zlib1g" {
        return Some("libz.so.1".into());
    }
    let stem = p.trim_end_matches(|c: char| c.is_ascii_digit());
    let version = &p[stem.len()..];
    let ok = stem.starts_with("lib") && stem.len() > 3 && !version.is_empty() && !stem.ends_with(['.', '-']);
    ok.then(|| format!("{stem}.so.{version}"))
}

/// The sonames `ldconfig -p` lists for this machine's architecture, read
/// once; None when ldconfig can't be run.
fn loader_cache() -> Option<&'static HashSet<String>> {
    static CACHE: OnceLock<Option<HashSet<String>>> = OnceLock::new();
    CACHE
        .get_or_init(|| {
            let arch = if cfg!(target_arch = "aarch64") { "AArch64" } else { "x86-64" };
            let out = ["ldconfig", "/sbin/ldconfig", "/usr/sbin/ldconfig"]
                .iter()
                .find_map(|cmd| std::process::Command::new(cmd).arg("-p").output().ok().filter(|o| o.status.success()))?;
            let libs: HashSet<String> = String::from_utf8_lossy(&out.stdout)
                .lines()
                .filter(|l| l.contains(arch))
                .filter_map(|l| l.split_whitespace().next().map(str::to_string))
                .collect();
            log::info!("loader cache: {} libraries for {arch}", libs.len());
            Some(libs)
        })
        .as_ref()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn package_names_to_sonames() {
        assert_eq!(soname("libasound2").as_deref(), Some("libasound.so.2"));
        assert_eq!(soname("libasound2t64").as_deref(), Some("libasound.so.2"));
        assert_eq!(soname("libudev1").as_deref(), Some("libudev.so.1"));
        assert_eq!(soname("libgmp10").as_deref(), Some("libgmp.so.10"));
        assert_eq!(soname("libstdc++6").as_deref(), Some("libstdc++.so.6"));
        assert_eq!(soname("zlib1g").as_deref(), Some("libz.so.1"));
        assert_eq!(soname("libfoo-2.0-0"), None);
        assert_eq!(soname("ncurses-bin"), None);
    }
}
