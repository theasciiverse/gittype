//! Copying text out of av. Terminals don't agree on a way for a program to
//! reach the clipboard: VTE (GNOME Terminal, Ptyxis) ignores OSC 52, which
//! kitty, Ghostty, WezTerm, foot and Windows Terminal take. So av sends
//! OSC 52 and also sets the system clipboard: through wl-copy, xclip or
//! xsel when one is installed, since they keep the text after av quits,
//! otherwise itself (arboard), holding it while av runs.

use std::io::Write;
use std::process::{Command, Stdio};

#[derive(Default)]
pub struct Clipboard {
    /// kept so the text stays available on Linux while av runs
    held: Option<arboard::Clipboard>,
}

impl Clipboard {
    /// Copy `text`, and say how it went for the status line.
    pub fn copy(&mut self, text: &str) -> String {
        osc52(text);
        let n = text.chars().count();
        match self.system(text) {
            Ok(how) => {
                log::info!("copied {n} characters with {how}");
                format!("copied {n} characters")
            }
            Err(e) => {
                log::warn!("clipboard: {e:#}; sent OSC 52 only");
                format!("sent {n} characters to the terminal's clipboard")
            }
        }
    }

    fn system(&mut self, text: &str) -> anyhow::Result<&'static str> {
        if cfg!(target_os = "linux") {
            let wayland = std::env::var_os("WAYLAND_DISPLAY").is_some();
            let x11 = std::env::var_os("DISPLAY").is_some();
            let tools: [(bool, &'static str, &[&str]); 3] = [
                (wayland, "wl-copy", &[]),
                (x11, "xclip", &["-selection", "clipboard"]),
                (x11, "xsel", &["--clipboard", "--input"]),
            ];
            for (usable, tool, args) in tools {
                if usable && pipe_to(tool, args, text).is_ok() {
                    return Ok(tool);
                }
            }
        }
        let held = match self.held.as_mut() {
            Some(c) => c,
            None => self.held.insert(arboard::Clipboard::new()?),
        };
        held.set_text(text)?;
        Ok("arboard")
    }
}

fn pipe_to(tool: &str, args: &[&str], text: &str) -> std::io::Result<()> {
    let mut child = Command::new(tool).args(args).stdin(Stdio::piped()).stdout(Stdio::null()).stderr(Stdio::null()).spawn()?;
    child.stdin.take().expect("piped").write_all(text.as_bytes())?;
    let status = child.wait()?;
    if status.success() { Ok(()) } else { Err(std::io::Error::other(format!("{tool} exited with {status}"))) }
}

/// The terminal's own clipboard, for terminals that take OSC 52 (and over ssh).
fn osc52(text: &str) {
    let mut out = std::io::stdout();
    let _ = write!(out, "\x1b]52;c;{}\x07", base64(text.as_bytes()));
    let _ = out.flush();
}

fn base64(bytes: &[u8]) -> String {
    const ABC: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut s = String::with_capacity(bytes.len().div_ceil(3) * 4);
    for chunk in bytes.chunks(3) {
        let n = chunk.iter().enumerate().fold(0u32, |n, (i, b)| n | (*b as u32) << (16 - 8 * i));
        for i in 0..4 {
            s.push(if i <= chunk.len() { ABC[(n >> (18 - 6 * i) & 63) as usize] as char } else { '=' });
        }
    }
    s
}

#[cfg(test)]
mod tests {
    #[test]
    fn base64_matches_the_standard() {
        assert_eq!(super::base64(b""), "");
        assert_eq!(super::base64(b"f"), "Zg==");
        assert_eq!(super::base64(b"fo"), "Zm8=");
        assert_eq!(super::base64(b"foo"), "Zm9v");
        assert_eq!(super::base64("Fanorona · å".as_bytes()), "RmFub3JvbmEgwrcgw6U=");
    }
}
