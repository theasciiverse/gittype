//! `installed.json` and `wishlist.json` in the state dir. Local only, never synced.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use anyhow::Result;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Installed {
    pub id: String,
    pub version: String,
    pub sha256: String,
    pub installed_at: String,
    #[serde(default)]
    pub kind: String,
    #[serde(default)]
    pub target: String,
    /// how to start it, kept so it still starts after the catalog drops it
    #[serde(default)]
    pub exec: String,
    #[serde(default)]
    pub args: Vec<String>,
}

/// Local play history per title, Steam-style; never leaves the machine.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Played {
    pub last_played: String,
    pub seconds: u64,
    pub sessions: u32,
}

#[derive(Clone, Default, PartialEq)]
struct Lists {
    installed: BTreeMap<String, Installed>,
    wishlist: BTreeSet<String>,
    played: BTreeMap<String, Played>,
}

pub struct State {
    dir: PathBuf,
    pub installed: BTreeMap<String, Installed>,
    pub wishlist: BTreeSet<String>,
    pub played: BTreeMap<String, Played>,
    /// the files as this process last read or wrote them, so a save writes
    /// only its own changes over whatever another av has saved since
    base: Lists,
    /// files that could not be read and were set aside, for the status line
    pub set_aside: Vec<String>,
}

/// One of the state files, or the default when it is missing. A file that
/// is there but can't be read is renamed to `<name>.bad-<time>` first, so
/// the next save doesn't write an empty list over the player's library.
fn read_kept<T: for<'de> Deserialize<'de> + Default>(path: &Path, set_aside: &mut Vec<String>) -> T {
    let Ok(s) = std::fs::read_to_string(path) else { return T::default() };
    match serde_json::from_str(&s) {
        Ok(v) => v,
        Err(e) => {
            let bad = path.with_extension(format!("json.bad-{}", now_rfc3339().replace(':', "")));
            log::warn!("{}: {e}; set aside as {}", path.display(), bad.display());
            if std::fs::rename(path, &bad).is_ok() {
                set_aside.push(bad.display().to_string());
            }
            T::default()
        }
    }
}

pub fn read<T: for<'de> Deserialize<'de> + Default>(path: &Path) -> T {
    read_kept(path, &mut Vec::new())
}

impl Lists {
    fn load(dir: &Path, set_aside: &mut Vec<String>) -> Lists {
        let installed: Vec<Installed> = read_kept(&dir.join("installed.json"), set_aside);
        Lists {
            installed: installed.into_iter().map(|i| (i.id.clone(), i)).collect(),
            wishlist: read_kept(&dir.join("wishlist.json"), set_aside),
            played: read_kept(&dir.join("played.json"), set_aside),
        }
    }
}

impl State {
    pub fn load(dir: &Path) -> Self {
        let mut set_aside = Vec::new();
        let l = Lists::load(dir, &mut set_aside);
        State {
            dir: dir.to_path_buf(),
            installed: l.installed.clone(),
            wishlist: l.wishlist.clone(),
            played: l.played.clone(),
            base: l,
            set_aside,
        }
    }

    /// Add a finished session to the title's play history; returns the session count.
    pub fn record_play(&mut self, id: &str, seconds: u64) -> u32 {
        let p = self.played.entry(id.to_string()).or_default();
        p.seconds += seconds;
        p.sessions += 1;
        p.last_played = now_rfc3339();
        p.sessions
    }

    fn mine(&self) -> Lists {
        Lists { installed: self.installed.clone(), wishlist: self.wishlist.clone(), played: self.played.clone() }
    }

    /// Write this process's changes. Another av may have saved since this one
    /// read the files (`av play` running for an hour while the TUI installs
    /// titles), so under a lock the files are read again and only what
    /// changed here is applied to them; the result is what this process
    /// sees from then on.
    pub fn save(&mut self) -> Result<()> {
        let mine = self.mine();
        if mine == self.base {
            return Ok(());
        }
        let _lock = Lock::take(&self.dir.join("state.lock"));
        let mut disk = Lists::load(&self.dir, &mut self.set_aside);
        for (id, i) in &mine.installed {
            if self.base.installed.get(id).is_none_or(|b| b.version != i.version || b.installed_at != i.installed_at) {
                disk.installed.insert(id.clone(), i.clone());
            }
        }
        for id in self.base.installed.keys().filter(|id| !mine.installed.contains_key(*id)) {
            disk.installed.remove(id);
        }
        disk.wishlist.extend(mine.wishlist.difference(&self.base.wishlist).cloned());
        for id in self.base.wishlist.difference(&mine.wishlist) {
            disk.wishlist.remove(id);
        }
        for (id, p) in &mine.played {
            let b = self.base.played.get(id).cloned().unwrap_or_default();
            if *p != b {
                let d = disk.played.entry(id.clone()).or_default();
                d.seconds += p.seconds.saturating_sub(b.seconds);
                d.sessions += p.sessions.saturating_sub(b.sessions);
                d.last_played = d.last_played.clone().max(p.last_played.clone());
            }
        }
        let list: Vec<&Installed> = disk.installed.values().collect();
        write_atomic(&self.dir.join("installed.json"), &serde_json::to_string_pretty(&list)?)?;
        write_atomic(&self.dir.join("wishlist.json"), &serde_json::to_string_pretty(&disk.wishlist)?)?;
        write_atomic(&self.dir.join("played.json"), &serde_json::to_string_pretty(&disk.played)?)?;
        (self.installed, self.wishlist, self.played) = (disk.installed.clone(), disk.wishlist.clone(), disk.played.clone());
        self.base = disk;
        Ok(())
    }
}

/// A lock file held while the state files are read and rewritten. It waits
/// up to two seconds for another av, and takes over a lock left behind by
/// one that died; saving without it beats not saving.
struct Lock(Option<PathBuf>);

impl Lock {
    fn take(path: &Path) -> Lock {
        for _ in 0..40 {
            match std::fs::File::create_new(path) {
                Ok(_) => return Lock(Some(path.to_path_buf())),
                Err(_) => {
                    let stale = std::fs::metadata(path)
                        .and_then(|m| m.modified())
                        .is_ok_and(|t| t.elapsed().is_ok_and(|e| e.as_secs() > 10));
                    if stale {
                        let _ = std::fs::remove_file(path);
                    } else {
                        std::thread::sleep(std::time::Duration::from_millis(50));
                    }
                }
            }
        }
        log::warn!("{} held too long; saving anyway", path.display());
        Lock(None)
    }
}

impl Drop for Lock {
    fn drop(&mut self) {
        if let Some(p) = &self.0 {
            let _ = std::fs::remove_file(p);
        }
    }
}

/// Write via a temp file and rename, so a crash never leaves half a file.
/// The temp name is this process's own, so two writers never share one.
pub fn write_atomic(path: &Path, body: &str) -> Result<()> {
    let tmp = path.with_extension(format!("tmp.{}", std::process::id()));
    std::fs::write(&tmp, body)?;
    std::fs::rename(&tmp, path)?;
    Ok(())
}

/// UTC timestamp in RFC 3339 form without pulling in a date crate.
pub fn now_rfc3339() -> String {
    let secs = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0);
    let (days, rem) = (secs.div_euclid(86_400), secs.rem_euclid(86_400));
    // civil-from-days, Howard Hinnant's algorithm
    let z = days + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = yoe + era * 400 + i64::from(m <= 2);
    format!("{y:04}-{m:02}-{d:02}T{:02}:{:02}:{:02}Z", rem / 3600, rem % 3600 / 60, rem % 60)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn installed(id: &str, version: &str) -> Installed {
        Installed { id: id.into(), version: version.into(), sha256: String::new(), installed_at: now_rfc3339(), kind: "game".into(), target: String::new(), exec: String::new(), args: Vec::new() }
    }

    fn dir(name: &str) -> PathBuf {
        let d = std::env::temp_dir().join(format!("av-state-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&d);
        std::fs::create_dir_all(&d).unwrap();
        d
    }

    #[test]
    fn two_avs_saving_keep_each_others_changes() {
        let d = dir("two");
        let mut setup = State::load(&d);
        setup.installed.insert("old".into(), installed("old", "1"));
        setup.save().unwrap();

        // `av play` loads, the TUI loads, both change things, the TUI saves first
        let (mut play, mut tui) = (State::load(&d), State::load(&d));
        tui.installed.insert("new".into(), installed("new", "1"));
        tui.installed.remove("old");
        tui.wishlist.insert("wanted".into());
        tui.save().unwrap();
        play.record_play("old", 60);
        play.record_play("other", 30);
        play.save().unwrap();

        let now = State::load(&d);
        assert!(now.installed.contains_key("new"), "the TUI's install survived av play's save");
        assert!(!now.installed.contains_key("old"), "and so did its uninstall");
        assert!(now.wishlist.contains("wanted"));
        assert_eq!(now.played["old"].seconds, 60);
        // play now sees what the TUI did, and its next session adds up
        assert!(play.installed.contains_key("new"));
        play.record_play("old", 40);
        play.save().unwrap();
        assert_eq!(State::load(&d).played["old"].seconds, 100);
        let _ = std::fs::remove_dir_all(&d);
    }

    #[test]
    fn an_unreadable_file_is_set_aside_not_emptied() {
        let d = dir("bad");
        std::fs::write(d.join("installed.json"), "[{ this is not json").unwrap();
        let mut s = State::load(&d);
        assert!(s.installed.is_empty());
        assert_eq!(s.set_aside.len(), 1);
        let kept: Vec<_> = std::fs::read_dir(&d).unwrap().flatten().filter(|e| e.file_name().to_string_lossy().contains(".bad-")).collect();
        assert_eq!(kept.len(), 1, "the broken file is kept beside");
        assert_eq!(std::fs::read_to_string(kept[0].path()).unwrap(), "[{ this is not json");
        s.wishlist.insert("x".into());
        s.save().unwrap();
        let _ = std::fs::remove_dir_all(&d);
    }

    #[test]
    fn nothing_changed_writes_nothing() {
        let d = dir("quiet");
        let mut s = State::load(&d);
        s.save().unwrap();
        assert!(!d.join("installed.json").exists());
        let _ = std::fs::remove_dir_all(&d);
    }

    #[test]
    fn timestamp_shape() {
        let t = super::now_rfc3339();
        assert_eq!(t.len(), 20);
        assert!(t.starts_with("20") && t.ends_with('Z'));
    }
}
