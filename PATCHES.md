# Changes from upstream

This is the Asciiverse edition of gittype, Yuji Ueki's (unhappychoice)
typing game that turns source code into challenges (Rust, Ratatui, 2025
on, MIT), from github.com/unhappychoice/gittype (the `upstream` remote) at
release v0.10.4 (`a29fd91`, 2026-10-31). The `asciiverse` branch carries
the changes below; the code changes are written to go upstream.

## Code

- **A plain folder of code plays, not only a git repository.** With no
  repository around the files, extraction stopped with "Git repository
  not found" and there were no challenges. The folder all the files share
  now stands in for the repository root
  (`source_code_parser.rs`).
- **Data in the state folder.** The database, logs, settings, themes and
  cloned repositories went to `~/.gittype`, built in four places. One
  function now picks `$XDG_STATE_HOME/gittype`, else
  `~/.local/state/gittype` (Application Support/gittype on macOS,
  `%LOCALAPPDATA%\gittype` on Windows); a player who already has
  `~/.gittype` keeps it. The help screen and the custom theme's
  description name the folder in use (`app_data_provider.rs`; tests and
  the help snapshot follow).
- **No update check when the build says so.** Built with
  `GITTYPE_NO_UPDATE_CHECK` set, gittype no longer asks GitHub for a newer
  release at start or shows its curl-to-update screen; av brings the
  updates (`game.rs`).
- **The screen is cleared without asking the terminal for the cursor.**
  ratatui's `Terminal::clear()` reads the cursor position first, on every
  change of screen, and the answer could arrive as stray input:
  "^[[24;65R" written over the screen, or keys in a challenge. The screen
  manager now clears the backend and resets ratatui's buffers itself
  (`screen_manager.rs`).

## Code to type

av starts every title in its install folder, so the archive carries
`samples/` beside the program: source from games in the Asciiverse
catalog (Four in a Row, av, TinyCols, Galaxis, blackjack-tui, Zoridor,
Intricacy, robotfindskitten), each folder with its own licence, listed in
`samples/README.md`. `asciiverse/make-samples.sh` rebuilds the set with
git archive from those repositories at fixed commits. Started from a
project of your own, gittype types that instead, as upstream.

## Packaging

- `.github/workflows/asciiverse-release.yml` (after ttyper's): Linux
  x86-64 and ARM64 static (musl) in Alpine, with libgit2, OpenSSL and
  SQLite built in; macOS x86-64 and ARM64; Windows x86-64; every archive
  with `samples/`; SHA256SUMS; built with `GITTYPE_NO_UPDATE_CHECK`; runs
  on `*-av.*` tags.
- `asciiverse/record.sh` and `asciiverse/drive.py` record
  `asciiverse/gittype.cast`, the 100x30 catalog preview: the program laid
  out as av installs it, and a driver that reads each challenge from the
  screen and types it at a human pace with the odd slip.

## Build fix (v0.10.4-av.2)

- **Rust pinned to 1.93** (`rust-toolchain.toml`, and `rust:1.93-alpine` in the
  release workflow). Rust 1.100.0 (2026-11-09) never finishes compiling the
  gittype crate: rustc sits at 100% CPU with little memory (reproduced in
  Docker, stopped after 6 minutes), where 1.93.1 builds the whole static
  binary in about 2 minutes. The first release run (v0.10.4-av.1) hung like
  that for over three hours on Linux and Windows and was cancelled.
- The workflow now has a 60-minute limit per build job, caches the Rust
  build on macOS and Windows, and publishes the release whenever the Linux
  builds succeed, adding macOS and Windows when they build too.
