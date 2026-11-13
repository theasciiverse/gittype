//! av's own updates. The catalog names the latest av in its top-level `av`
//! entry; when that is newer than this one, av says so, and how to update,
//! which depends on how this av was installed.

use std::sync::OnceLock;

use crate::catalog::AvRelease;

/// The catalog's av, when it is newer than this one.
pub fn newer(latest: Option<&AvRelease>) -> Option<&AvRelease> {
    latest.filter(|r| version(&r.version) > version(env!("CARGO_PKG_VERSION")))
}

/// A version for comparing: "0.9.10" after "0.9.9", " v1.0.0" as 1.0.0,
/// and a pre-release ("1.0.0-rc.1") before its release.
fn version(v: &str) -> (Vec<u64>, bool) {
    let v = v.trim().trim_start_matches(['v', 'V']);
    let (num, pre) = match v.split_once(['-', '+']) {
        Some((n, rest)) => (n, v[n.len()..].starts_with('-') && !rest.is_empty()),
        None => (v, false),
    };
    (num.split('.').map(|p| p.parse().unwrap_or(0)).collect(), !pre)
}

/// The command that updates this av, worked out once. The install line
/// updates both the .deb and a per-user install (it asks which); cargo
/// builds its own.
pub fn how() -> &'static str {
    static HOW: OnceLock<&'static str> = OnceLock::new();
    HOW.get_or_init(|| {
        if from_cargo() {
            "cargo install --locked --git https://github.com/theasciiverse/av"
        } else if cfg!(windows) {
            "irm https://asciiverse.sh/install.ps1 | iex"
        } else if on_path("curl") || !on_path("wget") {
            // Ubuntu's desktop has wget but not curl; macOS has curl
            "curl -fsSL https://asciiverse.sh/install.sh | sh"
        } else {
            "wget -qO- https://asciiverse.sh/install.sh | sh"
        }
    })
}

/// "av 0.9.9 (2026-11-02) is out; you have 0.9.8", the one wording for the
/// menu row's dialog, help and the shell.
pub fn headline(r: &AvRelease) -> String {
    let date = if r.date.is_empty() { String::new() } else { format!(" ({})", r.date) };
    format!("av {}{date} is out; you have {}", r.version, env!("CARGO_PKG_VERSION"))
}

/// One line for the shell: the headline and the command.
pub fn notice(r: &AvRelease) -> String {
    format!("{}. To update: {}", headline(r), how())
}

/// Whether this av is the one `cargo install` put in cargo's bin folder.
fn from_cargo() -> bool {
    let Ok(exe) = std::env::current_exe().and_then(|p| p.canonicalize()) else { return false };
    let home = std::env::var_os("CARGO_HOME")
        .map(std::path::PathBuf::from)
        .or_else(|| directories::BaseDirs::new().map(|b| b.home_dir().join(".cargo")));
    home.and_then(|h| h.join("bin").canonicalize().ok()).is_some_and(|bin| exe.starts_with(bin))
}

fn on_path(program: &str) -> bool {
    std::env::var_os("PATH").is_some_and(|p| std::env::split_paths(&p).any(|d| d.join(program).is_file()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn versions_compare_by_number() {
        assert!(version("0.9.10") > version("0.9.9"));
        assert!(version("1.0.0") > version("0.99.0"));
        assert_eq!(version("v1.2.3"), version("1.2.3"));
        assert_eq!(version(" 1.2.3\n"), version("1.2.3"));
        assert!(version("1.0.0") > version("1.0.0-rc.1"), "a release is after its candidate");
        assert!(version("1.0.0-rc.1") > version("0.9.9"));
        assert_eq!(version("1.0.0+build.5"), version("1.0.0"));
        let release = |v: &str| AvRelease { version: v.into(), ..Default::default() };
        assert!(newer(Some(&release("999.0.0"))).is_some());
        assert!(newer(Some(&release(&format!("v{}", env!("CARGO_PKG_VERSION"))))).is_none());
        assert!(newer(Some(&release("0.0.1"))).is_none());
        assert!(newer(None).is_none());
    }
}
