//! The four per-OS buckets from the design (install, state, cache, config),
//! named after the brand the way Steam names its folder, rather than after
//! the binary:
//!
//! | | Linux | macOS | Windows |
//! |---|---|---|---|
//! | install, state | `~/.local/share/asciiverse` | `~/Library/Application Support/Asciiverse` | `%LOCALAPPDATA%\Asciiverse` |
//! | cache | `~/.cache/asciiverse` | `~/Library/Caches/Asciiverse` | `%LOCALAPPDATA%\Asciiverse\cache` |
//! | config | `~/.config/asciiverse` | `~/Library/Application Support/Asciiverse` | `%APPDATA%\Asciiverse` |

use std::path::PathBuf;

use anyhow::{Context, Result};
use directories::BaseDirs;

pub struct Paths {
    /// installed titles: `<install>/{games,programs,movies}/<id>/`
    pub install: PathBuf,
    /// `installed.json`, `wishlist.json`
    pub state: PathBuf,
    /// `catalog.json`, downloaded archives, casts, `av.log`; safe to delete
    pub cache: PathBuf,
    /// av's own preferences (nothing yet)
    #[allow(dead_code)]
    pub config: PathBuf,
    /// where installed binaries are symlinked on Linux and macOS (the pipx convention)
    pub bin: Option<PathBuf>,
}

/// The folder name: lowercase where the platform's own folders are (XDG),
/// the brand's spelling elsewhere.
const NAME: &str = if cfg!(any(target_os = "macos", windows)) { "Asciiverse" } else { "asciiverse" };

/// A path for display, with the home directory shortened to `~` on Linux
/// and macOS; Windows has no `~`, so there it is shown in full.
pub fn tilde(p: &std::path::Path) -> String {
    if cfg!(windows) {
        return p.display().to_string();
    }
    match BaseDirs::new().and_then(|b| p.strip_prefix(b.home_dir()).ok().map(|r| r.to_path_buf())) {
        Some(rest) => format!("~/{}", rest.display()),
        None => p.display().to_string(),
    }
}

impl Paths {
    pub fn new() -> Result<Self> {
        let b = BaseDirs::new().context("no home directory")?;
        // data_local_dir, never data_dir: on Windows binaries and install state
        // must not sync through a roaming profile
        let data = b.data_local_dir().join(NAME);
        let cache = if cfg!(windows) { data.join("cache") } else { b.cache_dir().join(NAME) };
        let config = b.config_dir().join(NAME);
        let bin = if cfg!(unix) { Some(b.home_dir().join(".local/bin")) } else { None };
        let p = Paths { install: data.clone(), state: data, cache, config, bin };
        std::fs::create_dir_all(&p.state)?;
        std::fs::create_dir_all(&p.cache)?;
        Ok(p)
    }

    pub fn title_dir(&self, kind: &str, id: &str) -> PathBuf {
        self.install.join(format!("{kind}s")).join(id)
    }

    /// av's own log, which a problem report should come with.
    pub fn log(&self) -> PathBuf {
        self.cache.join("av.log")
    }

    /// Where a title is installed: under the kind it was installed as, which
    /// the catalog may have changed since, else the catalog's kind.
    pub fn installed_dir(&self, t: &crate::catalog::Title, installed: Option<&crate::state::Installed>) -> PathBuf {
        let kind = installed.map(|i| i.kind.as_str()).filter(|k| !k.is_empty()).unwrap_or(&t.kind);
        self.title_dir(kind, &t.id)
    }

    /// Where an uninstalled title's own files (saves, scores, settings it
    /// wrote next to itself) wait for a reinstall.
    pub fn saves(&self, id: &str) -> PathBuf {
        self.state.join("saves").join(id)
    }

    /// Whether `~/.local/bin` is on `$PATH`; `av` says so, it never edits rc files.
    pub fn bin_on_path(&self) -> bool {
        let Some(bin) = &self.bin else { return true };
        std::env::var_os("PATH")
            .map(|p| std::env::split_paths(&p).any(|d| d == *bin))
            .unwrap_or(false)
    }
}
