//! What carries over between runs: the last level and board picked, and
//! games won and lost against each computer level. Kept in
//! $XDG_DATA_HOME/four-in-a-row/state (~/.local/share/four-in-a-row/state,
//! or %APPDATA%\four-in-a-row\state on Windows) as plain `key=value` lines.

use crate::engine::LEVELS;
use std::path::PathBuf;

#[derive(Default, Debug)]
pub struct Store {
    pub level: Option<String>,
    pub board: Option<String>,
    pub muted: bool,
    /// won and lost against each level, in LEVELS order
    pub record: [(u32, u32); 4],
}

fn path() -> Option<PathBuf> {
    let env = |k: &str| std::env::var_os(k).filter(|v| !v.is_empty()).map(PathBuf::from);
    let base = env("XDG_DATA_HOME")
        .or_else(|| if cfg!(windows) { env("APPDATA") } else { None })
        .or_else(|| env("HOME").map(|h| h.join(".local/share")))?;
    Some(base.join("four-in-a-row").join("state"))
}

impl Store {
    pub fn load() -> Store {
        let mut s = Store::default();
        let Some(text) = path().and_then(|p| std::fs::read_to_string(p).ok()) else { return s };
        for line in text.lines() {
            let Some((k, v)) = line.split_once('=') else { continue };
            let (k, v) = (k.trim(), v.trim());
            match k {
                "level" => s.level = Some(v.to_string()),
                "board" => s.board = Some(v.to_string()),
                "muted" => s.muted = v == "true",
                _ => {
                    for (i, l) in LEVELS.iter().enumerate() {
                        let name = l.name().to_ascii_lowercase();
                        if k == format!("won_{name}") {
                            s.record[i].0 = v.parse().unwrap_or(0);
                        } else if k == format!("lost_{name}") {
                            s.record[i].1 = v.parse().unwrap_or(0);
                        }
                    }
                }
            }
        }
        s
    }

    pub fn save(&self) {
        let Some(p) = path() else { return };
        let mut text = format!(
            "level={}\nboard={}\nmuted={}\n",
            self.level.as_deref().unwrap_or("Normal"),
            self.board.as_deref().unwrap_or("Classic"),
            self.muted
        );
        for (i, l) in LEVELS.iter().enumerate() {
            let name = l.name().to_ascii_lowercase();
            text += &format!("won_{name}={}\nlost_{name}={}\n", self.record[i].0, self.record[i].1);
        }
        let result = p.parent().map_or(Ok(()), std::fs::create_dir_all).and_then(|_| std::fs::write(&p, text));
        if let Err(e) = result {
            log::warn!("can't save to {}: {e}", p.display());
        }
    }
}
