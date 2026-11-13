//! Whether this machine can show the glyphs a title draws with, so the
//! detail pane only warns when it can't. Terminals fill in characters their
//! own font lacks from any installed font, so the question is whether any
//! font has them.

use std::sync::OnceLock;

/// Where the site explains fonts and glyphs.
pub const HELP_URL: &str = "https://asciiverse.sh/get-av.html#fonts";

/// True when the glyph set is known to be missing. Octants are never
/// missing in that sense: titles fall back to Braille and half blocks
/// without them. Names av cannot check are given the benefit of the doubt.
pub fn missing(glyph: &str) -> bool {
    static BRAILLE: OnceLock<bool> = OnceLock::new();
    match glyph {
        "braille" => !*BRAILLE.get_or_init(|| has_font_with(0x2800)),
        _ => false,
    }
}

/// Does any installed font have this character? Linux and the BSDs ask
/// fontconfig; when it can't be asked, the answer is yes. macOS and Windows
/// ship fallback fonts with Braille (Apple Braille, Segoe UI Symbol).
fn has_font_with(c: u32) -> bool {
    if cfg!(any(target_os = "macos", windows)) {
        return true;
    }
    match std::process::Command::new("fc-list").arg(format!(":charset={c:x}")).arg("family").output() {
        Ok(o) if o.status.success() => {
            let found = !o.stdout.iter().all(u8::is_ascii_whitespace);
            log::info!("fonts with U+{c:04X}: {}", if found { "found" } else { "none" });
            found
        }
        _ => true,
    }
}
