use std::path::PathBuf;

use crate::Result;

pub trait AppDataProvider {
    fn get_app_data_dir() -> Result<PathBuf> {
        #[cfg(not(feature = "test-mocks"))]
        {
            use crate::GitTypeError;

            let data_dir = if cfg!(debug_assertions) {
                std::env::current_dir()
                    .map_err(|e| {
                        GitTypeError::ExtractionFailed(format!(
                            "Could not get current directory: {}",
                            e
                        ))
                    })?
                    .join(".config")
            } else {
                release_data_dir()?
            };

            std::fs::create_dir_all(&data_dir)?;
            Ok(data_dir)
        }

        #[cfg(feature = "test-mocks")]
        {
            Ok(PathBuf::from("/tmp/test"))
        }
    }
}

/// Where a release build keeps its database, logs, settings, themes and
/// cloned repositories. An existing ~/.gittype is used as it is; otherwise
/// the platform's folder for program state: $XDG_STATE_HOME/gittype, else
/// ~/.local/state/gittype on Linux, Application Support/gittype on macOS,
/// %LOCALAPPDATA%\gittype on Windows.
pub fn release_data_dir() -> Result<PathBuf> {
    use crate::GitTypeError;

    let home_dir = dirs::home_dir();
    if let Some(home) = &home_dir {
        let legacy = home.join(".gittype");
        if legacy.is_dir() {
            return Ok(legacy);
        }
    }
    if let Some(xdg) = std::env::var_os("XDG_STATE_HOME").map(PathBuf::from) {
        if xdg.is_absolute() {
            return Ok(xdg.join("gittype"));
        }
    }
    let base = if cfg!(target_os = "macos") {
        dirs::data_dir()
    } else if cfg!(windows) {
        dirs::data_local_dir()
    } else {
        dirs::state_dir().or_else(|| home_dir.as_ref().map(|h| h.join(".local").join("state")))
    };
    base.map(|b| b.join("gittype")).ok_or_else(|| {
        GitTypeError::ExtractionFailed("Could not determine home directory".to_string())
    })
}

/// The data folder as shown to the player, with the home folder as ~.
pub fn display_data_dir() -> String {
    if cfg!(feature = "test-mocks") {
        // the same text on every machine, for snapshot tests
        return "~/.local/state/gittype".to_string();
    }
    let dir = release_data_dir().unwrap_or_else(|_| PathBuf::from("~/.gittype"));
    if let Some(home) = dirs::home_dir() {
        if let Ok(rest) = dir.strip_prefix(&home) {
            return format!("~/{}", rest.display());
        }
    }
    dir.display().to_string()
}
