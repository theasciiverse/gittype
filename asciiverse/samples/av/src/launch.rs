//! Running a title: av hands it the terminal and gets it back afterwards.
//!
//! While the title runs, av ignores Ctrl+C and Ctrl+\ the way a shell does,
//! so a title that leaves raw mode (telnet, a crash) cannot take av down with
//! it; the title itself gets the default handlers back before it starts. Its
//! stderr goes to a log in the cache, so messages neither scramble the screen
//! nor get lost. Afterwards the terminal is put back exactly as av left it,
//! whatever state the title exited in.

use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::time::Instant;

pub struct Outcome {
    pub status: std::io::Result<ExitStatus>,
    pub seconds: u64,
    pub log: Option<PathBuf>,
}

/// Open a link in the browser with the system's own opener, detached and
/// silent, so nothing it prints lands on av's screen.
pub fn open_url(url: &str) -> std::io::Result<()> {
    let mut cmd = if cfg!(windows) {
        // not through cmd, which would read & | ^ % in the URL as its own
        let mut c = Command::new("rundll32");
        c.args(["url.dll,FileProtocolHandler", url]);
        c
    } else if cfg!(target_os = "macos") {
        let mut c = Command::new("open");
        c.arg(url);
        c
    } else {
        let mut c = Command::new("xdg-open");
        c.arg(url);
        c
    };
    let mut child = cmd.stdin(Stdio::null()).stdout(Stdio::null()).stderr(Stdio::null()).spawn()?;
    // reap it in the background so it never lingers as a zombie
    std::thread::spawn(move || child.wait());
    Ok(())
}

pub fn run(mut cmd: Command, log: Option<PathBuf>) -> Outcome {
    if let Some(path) = &log {
        if let Some(dir) = path.parent() {
            let _ = std::fs::create_dir_all(dir);
        }
        match std::fs::File::create(path) {
            Ok(mut f) => {
                let _ = writeln!(f, "# {} started by av at {}", cmd.get_program().to_string_lossy(), crate::state::now_rfc3339());
                cmd.stderr(Stdio::from(f));
            }
            Err(e) => log::warn!("cannot open {}: {e}", path.display()),
        }
    }
    let guard = Guard::new(&mut cmd);
    let start = Instant::now();
    let status = cmd.status();
    let seconds = start.elapsed().as_secs();
    drop(guard);
    Outcome { status, seconds, log }
}

/// The last `n` lines of a log, for the crash dialog.
pub fn tail(path: &Path, n: usize) -> String {
    let text = std::fs::read_to_string(path).unwrap_or_default();
    let lines: Vec<&str> = text.lines().filter(|l| !l.starts_with("# ") && !l.trim().is_empty()).collect();
    lines[lines.len().saturating_sub(n)..].join("\n")
}

/// Human description of how a title ended, or None for a clean exit.
pub fn failure(status: &ExitStatus) -> Option<String> {
    if status.success() {
        return None;
    }
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(sig) = status.signal() {
            let name = match sig {
                libc::SIGSEGV => "a segmentation fault (SIGSEGV)".to_string(),
                libc::SIGABRT => "an abort (SIGABRT)".to_string(),
                libc::SIGKILL => "being killed (SIGKILL)".to_string(),
                libc::SIGTERM => "being terminated (SIGTERM)".to_string(),
                libc::SIGINT => "Ctrl+C (SIGINT)".to_string(),
                s => format!("signal {s}"),
            };
            return Some(format!("stopped by {name}"));
        }
    }
    Some(match status.code() {
        Some(c) => format!("exited with code {c}"),
        None => "exited abnormally".into(),
    })
}

/// Saves the terminal mode and signal handlers on creation and restores
/// both, plus a full reset of terminal modes, when dropped.
struct Guard {
    #[cfg(unix)]
    termios: Option<libc::termios>,
    #[cfg(unix)]
    old_int: libc::sighandler_t,
    #[cfg(unix)]
    old_quit: libc::sighandler_t,
}

impl Guard {
    fn new(cmd: &mut Command) -> Self {
        #[cfg(unix)]
        {
            use std::os::unix::process::CommandExt;
            // SAFETY: tcgetattr writes into a zeroed termios we own; signal()
            // swaps process-wide handlers, which is what we want while waiting.
            let termios = unsafe {
                let mut t: libc::termios = std::mem::zeroed();
                (libc::tcgetattr(libc::STDIN_FILENO, &mut t) == 0).then_some(t)
            };
            let (old_int, old_quit) = unsafe { (libc::signal(libc::SIGINT, libc::SIG_IGN), libc::signal(libc::SIGQUIT, libc::SIG_IGN)) };
            // ignored signals stay ignored across exec, so give the title the defaults back
            // SAFETY: signal() is async-signal-safe and may be called between fork and exec
            unsafe {
                cmd.pre_exec(|| {
                    libc::signal(libc::SIGINT, libc::SIG_DFL);
                    libc::signal(libc::SIGQUIT, libc::SIG_DFL);
                    Ok(())
                });
            }
            Guard { termios, old_int, old_quit }
        }
        #[cfg(windows)]
        {
            // Ctrl+C reaches every process on the console: av ignores it
            // while the title runs, and the title keeps its own handling
            // SAFETY: registering a handler function that only returns TRUE
            unsafe { SetConsoleCtrlHandler(Some(ignore_ctrl), 1) };
            let _ = cmd;
            Guard {}
        }
        #[cfg(not(any(unix, windows)))]
        {
            let _ = cmd;
            Guard {}
        }
    }
}

impl Drop for Guard {
    fn drop(&mut self) {
        // undo anything a title may have left on: alternate screen, hidden
        // cursor, colors, mouse reporting, bracketed paste, focus events,
        // kitty keyboard flags, application keypad, cursor shape
        let reset = "\x1b[?1049l\x1b[?25h\x1b[0m\x1b[?1000l\x1b[?1002l\x1b[?1003l\x1b[?1006l\x1b[?1015l\
                     \x1b[?2004l\x1b[?1004l\x1b[<99u\x1b>\x1b[0 q";
        let mut out = std::io::stdout();
        let _ = out.write_all(reset.as_bytes());
        let _ = out.flush();
        #[cfg(unix)]
        // SAFETY: restoring the saved termios and the previous handlers
        unsafe {
            if let Some(t) = &self.termios {
                libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, t);
            }
            libc::signal(libc::SIGINT, self.old_int);
            libc::signal(libc::SIGQUIT, self.old_quit);
        }
        #[cfg(windows)]
        // SAFETY: removing the handler added in Guard::new
        unsafe {
            SetConsoleCtrlHandler(Some(ignore_ctrl), 0);
        }
    }
}

#[cfg(windows)]
unsafe extern "system" {
    fn SetConsoleCtrlHandler(handler: Option<unsafe extern "system" fn(u32) -> i32>, add: i32) -> i32;
}

/// Handles Ctrl+C and Ctrl+Break by doing nothing, so av lives on.
#[cfg(windows)]
unsafe extern "system" fn ignore_ctrl(_event: u32) -> i32 {
    1
}

/// Lera, the Asciiverse MUD client: installed through av as a program,
/// or anywhere on PATH.
pub fn lera(paths: &crate::paths::Paths) -> Option<PathBuf> {
    let name = if cfg!(windows) { "lera.exe" } else { "lera" };
    crate::install::find_exec(&paths.title_dir("program", "lera"), name).or_else(|| {
        let path = std::env::var_os("PATH")?;
        std::env::split_paths(&path).map(|d| d.join(name)).find(|p| p.is_file())
    })
}

/// The command that starts a title: its binary with the catalog's arguments,
/// run from its install folder, or for a MUD, Lera (telnet as the fallback).
/// The log is where the title's stderr goes, unless the title keeps it on
/// the terminal (`stderr = "terminal"` in its `[exec]`).
pub fn command(paths: &crate::paths::Paths, t: &crate::catalog::Title, installed: Option<&crate::state::Installed>) -> Result<(Command, Option<PathBuf>), String> {
    let cmd = if let Some(c) = &t.connect {
        // Lera finds the world by id in the same cached catalog; telnet is the fallback
        if let Some(lera) = lera(paths) {
            log::info!("connect {} with {}", t.id, lera.display());
            let mut cmd = Command::new(lera);
            cmd.arg(&t.id);
            cmd
        } else {
            log::info!("connect {} {}:{} with telnet", t.id, c.host, c.port);
            let mut cmd = Command::new("telnet");
            cmd.arg(&c.host).arg(c.port.to_string());
            cmd
        }
    } else {
        let dir = paths.installed_dir(t, installed);
        let Some(exe) = crate::install::find_exec(&dir, &t.exec_name()) else {
            return Err(format!("{} is not installed correctly; reinstall it", t.title));
        };
        log::info!("launch {} {}", t.id, exe.display());
        let mut cmd = Command::new(&exe);
        cmd.args(&t.exec.args).current_dir(&dir);
        cmd
    };
    let log = (t.exec.stderr.as_deref() != Some("terminal")).then(|| paths.cache.join("logs").join(format!("{}.log", t.id)));
    Ok((cmd, log))
}

/// "23 min", "1 h 05 min", "40 s"
pub fn duration(secs: u64) -> String {
    match secs {
        s if s < 60 => format!("{s} s"),
        s if s < 3600 => format!("{} min", s / 60),
        s => format!("{} h {:02} min", s / 3600, s % 3600 / 60),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn durations() {
        assert_eq!(duration(40), "40 s");
        assert_eq!(duration(23 * 60 + 5), "23 min");
        assert_eq!(duration(3900), "1 h 05 min");
    }

    #[cfg(unix)]
    #[test]
    fn reports_exit_codes_and_logs_stderr() {
        let log = std::env::temp_dir().join(format!("av-launch-test-{}.log", std::process::id()));
        let mut cmd = Command::new("sh");
        cmd.args(["-c", "echo boom >&2; exit 3"]);
        let o = run(cmd, Some(log.clone()));
        let st = o.status.unwrap();
        assert_eq!(failure(&st).as_deref(), Some("exited with code 3"));
        assert_eq!(tail(&log, 5), "boom");
        let _ = std::fs::remove_file(&log);
    }

    #[cfg(unix)]
    #[test]
    fn title_gets_default_sigint_back() {
        // the title sends itself SIGINT; with the default handler restored it dies of it
        let mut cmd = Command::new("sh");
        cmd.args(["-c", "kill -INT $$; sleep 1"]);
        let st = run(cmd, None).status.unwrap();
        assert_eq!(failure(&st).as_deref(), Some("stopped by Ctrl+C (SIGINT)"));
    }
}
