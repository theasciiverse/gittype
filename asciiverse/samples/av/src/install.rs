//! Download, checksum, extract, link. Runs on a worker thread and reports
//! progress over a channel. Every archive is checked against the catalog's
//! sha256 before anything is extracted.

use std::collections::HashSet;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::Sender;

use anyhow::{Context, Result, bail};
use sha2::{Digest, Sha256};

use crate::catalog::{self, Title};
use crate::state::Installed;

/// A worker's messages, each with its job number, so a late message from a
/// download the player cancelled never touches a newer one.
pub enum Progress {
    Bytes { job: u64, done: u64, total: u64 },
    Status { job: u64, msg: String },
    Done { job: u64, id: String, installed: Installed },
    Failed { job: u64, id: String, error: String },
}

pub struct Job {
    pub title: Title,
    pub catalog_url: String,
    pub cache: PathBuf,
    pub dest: PathBuf,
    /// the title's own files kept from an uninstall, put back by this install
    pub saves: PathBuf,
    pub bin: Option<PathBuf>,
    /// the install folder under the kind it was installed as, when the
    /// catalog has moved the title to another kind since: moved to `dest`
    pub previous: Option<PathBuf>,
    /// set from outside to stop the download; checked per chunk, and once
    /// more before the new install replaces the old
    pub cancel: Option<Arc<AtomicBool>>,
    pub job: u64,
}

pub fn spawn(job: Job, tx: Sender<Progress>) {
    std::thread::spawn(move || {
        let (id, n) = (job.title.id.clone(), job.job);
        let msg = match run(&job, &tx) {
            Ok(installed) => Progress::Done { job: n, id, installed },
            Err(e) if e.to_string() == "cancelled" => {
                log::info!("install {id} cancelled");
                Progress::Failed { job: n, id, error: "cancelled".into() }
            }
            Err(e) => {
                log::error!("install {id} failed: {e:#}");
                Progress::Failed { job: n, id, error: format!("{e:#}") }
            }
        };
        let _ = tx.send(msg);
    });
}

fn run(job: &Job, tx: &Sender<Progress>) -> Result<Installed> {
    let t = &job.title;
    let (target, plat) = catalog::pick_platform(t).context("no build for this platform")?;
    let url = catalog::resolve(&job.catalog_url, &plat.url);
    let downloads = job.cache.join("downloads");
    std::fs::create_dir_all(&downloads)?;
    // named by title and target, not by the URL, which may end in ?token=
    // or /download; the format is read from the file itself
    let archive = downloads.join(format!("{}-{target}", t.id));
    // gone however this ends: installed, failed or cancelled
    let _cleanup = Cleanup(vec![archive.clone()]);
    log::info!("install {}: {} from {url}", t.id, target);

    // download while hashing
    let status = |msg: &str| {
        let _ = tx.send(Progress::Status { job: job.job, msg: msg.into() });
    };
    let cancelled = || job.cancel.as_ref().is_some_and(|c| c.load(Ordering::Relaxed));
    status("downloading");
    // a connection that goes silent fails in the end instead of hanging:
    // ten minutes, and the time the archive takes at 20 kB/s
    let patience = std::time::Duration::from_secs(600 + plat.size / 20_000);
    let mut resp = catalog::http()
        .get(&url)
        .config()
        .timeout_recv_body(Some(patience))
        .build()
        .call()
        .with_context(|| format!("download {url}"))?;
    let total = resp.body().content_length().unwrap_or(plat.size);
    let mut reader = resp.body_mut().as_reader();
    let mut file = std::fs::File::create(&archive)?;
    let mut hasher = Sha256::new();
    let (mut buf, mut done) = (vec![0u8; 64 * 1024], 0u64);
    loop {
        if cancelled() {
            bail!("cancelled");
        }
        let n = reader.read(&mut buf)?;
        if n == 0 {
            break;
        }
        hasher.update(&buf[..n]);
        file.write_all(&buf[..n])?;
        done += n as u64;
        let _ = tx.send(Progress::Bytes { job: job.job, done, total });
    }
    file.sync_all()?;
    let got = hex(&hasher.finalize());
    if !got.eq_ignore_ascii_case(&plat.sha256) {
        bail!("checksum mismatch: the catalog says {}, the download is {got}. Not installed.", plat.sha256);
    }
    status("checksum ok, extracting");

    // extract into a fresh sibling dir, then swap it in, so a failed update
    // never leaves a half-written install
    let parent = job.dest.parent().context("bad install path")?;
    std::fs::create_dir_all(parent)?;
    // a title the catalog moved to another kind: its install, saves and
    // all, moves along before the update
    if let Some(prev) = job.previous.as_ref().filter(|p| *p != &job.dest) {
        recover(prev)?;
        if prev.exists() && !job.dest.exists() {
            log::info!("{} moves from {} to {}", t.id, prev.display(), job.dest.display());
            std::fs::rename(prev, &job.dest)?;
        }
    }
    recover(&job.dest)?;
    let staging = sibling(&job.dest, "new");
    // only ever the archive's files: the title's own are never moved here
    let _ = remove_all(&staging);
    std::fs::create_dir_all(&staging)?;
    let _staging = Cleanup(vec![staging.clone()]);
    extract(&archive, &staging)?;
    let exe = find_exec(&staging, &t.exec_name()).with_context(|| format!("{} not found in the archive", t.exec_name()))?;
    set_executable(&exe)?;
    write_shipped(&staging)?;
    if cancelled() {
        bail!("cancelled");
    }
    swap_in(&staging, &job.dest)?;
    // files the title wrote before an uninstall come back with it
    if job.saves.is_dir() {
        carry_over(&job.saves, &job.dest, None).context("put back the files kept at uninstall")?;
        let _ = remove_all(&job.saves);
    }
    let exe = job.dest.join(exe.strip_prefix(&staging)?);

    if let Some(bin) = &job.bin
        && let Some(note) = link(bin, t, &exe, &job.dest)?
    {
        status(&note);
    }
    Ok(Installed {
        id: t.id.clone(),
        version: t.version.clone(),
        sha256: got,
        installed_at: crate::state::now_rfc3339(),
        kind: t.kind.clone(),
        target: target.into(),
        exec: t.exec.names.values().find_map(|v| v.as_str().map(String::from)).unwrap_or_default(),
        args: t.exec.args.clone(),
    })
}

/// Files and folders removed when this goes out of scope, whatever happened.
struct Cleanup(Vec<PathBuf>);

impl Drop for Cleanup {
    fn drop(&mut self) {
        for p in &self.0 {
            if p.is_dir() {
                let _ = remove_all(p);
            } else {
                let _ = std::fs::remove_file(p);
            }
        }
    }
}

/// Unpack a `.tar.gz` (Linux and macOS) or `.zip` (Windows), told apart by
/// their first bytes. Both refuse entries that would land outside `dest`.
fn extract(archive: &Path, dest: &Path) -> Result<()> {
    let mut magic = [0u8; 4];
    let n = std::fs::File::open(archive)?.read(&mut magic)?;
    let file = std::fs::File::open(archive)?;
    match &magic[..n] {
        [0x1f, 0x8b, ..] => tar::Archive::new(flate2::read::GzDecoder::new(file)).unpack(dest).context("extract tar.gz")?,
        [b'P', b'K', 3, 4] => zip::ZipArchive::new(file).and_then(|mut z| z.extract(dest)).context("extract zip")?,
        _ => bail!("the download is neither a .tar.gz nor a .zip, the two archives av unpacks"),
    }
    Ok(())
}

/// The list of the archive's files, kept in the install folder, so an update
/// or an uninstall can tell them from the files the title wrote itself.
const SHIPPED: &str = ".av-shipped";

/// `.<id>.new` or `.<id>.old` beside an install folder.
fn sibling(dest: &Path, what: &str) -> PathBuf {
    let id = dest.file_name().map(|n| n.to_string_lossy().into_owned()).unwrap_or_default();
    dest.with_file_name(format!(".{id}.{what}"))
}

fn write_shipped(dir: &Path) -> Result<()> {
    let mut files = Vec::new();
    walk(dir, Path::new(""), &mut files)?;
    let list: String = files.iter().map(|p| format!("{}\n", p.to_string_lossy().replace('\\', "/"))).collect();
    std::fs::write(dir.join(SHIPPED), list)?;
    Ok(())
}

fn walk(dir: &Path, rel: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    for entry in std::fs::read_dir(dir.join(rel))? {
        let entry = entry?;
        let r = rel.join(entry.file_name());
        if entry.file_type()?.is_dir() { walk(dir, &r, out)? } else { out.push(r) }
    }
    Ok(())
}

/// The archive's files as recorded at install, or None for an install made
/// before av kept the list.
fn shipped(dir: &Path) -> Option<HashSet<PathBuf>> {
    let s = std::fs::read_to_string(dir.join(SHIPPED)).ok()?;
    Some(s.lines().filter(|l| !l.is_empty()).map(PathBuf::from).collect())
}

/// Put `staging` in place of `dest`. The old install is moved aside, not
/// deleted, until the new one is in and the title's own files are carried
/// over; if the swap fails, the old one goes back.
fn swap_in(staging: &Path, dest: &Path) -> Result<()> {
    if !dest.exists() {
        std::fs::rename(staging, dest)?;
        return Ok(());
    }
    let old = sibling(dest, "old");
    std::fs::rename(dest, &old).context("move the previous install aside (is the title running?)")?;
    if let Err(e) = std::fs::rename(staging, dest) {
        let _ = std::fs::rename(&old, dest);
        return Err(e).context("put the new install in place");
    }
    finish_swap(&old, dest)
}

/// The title's own files from the old install into the new one, then the
/// old one away. Until that has worked, the old folder stays, and the next
/// install or update finishes the job (see `recover`).
fn finish_swap(old: &Path, dest: &Path) -> Result<()> {
    carry_over(old, dest, shipped(old).as_ref()).context("keep the title's own files from the previous install")?;
    // the new install is in and complete: a leftover is tidied next time
    if let Err(e) = remove_all(old) {
        log::warn!("could not remove {}: {e}", old.display());
    }
    Ok(())
}

/// remove_dir_all, also through folders an archive shipped read-only.
fn remove_all(dir: &Path) -> std::io::Result<()> {
    match std::fs::remove_dir_all(dir) {
        Err(e) if e.kind() != std::io::ErrorKind::NotFound => {
            make_writable(dir);
            std::fs::remove_dir_all(dir)
        }
        r => r,
    }
}

fn make_writable(dir: &Path) {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        if std::fs::symlink_metadata(dir).is_ok_and(|m| m.is_dir()) {
            let _ = std::fs::set_permissions(dir, std::fs::Permissions::from_mode(0o755));
            for e in std::fs::read_dir(dir).into_iter().flatten().flatten() {
                make_writable(&e.path());
            }
        }
    }
    #[cfg(not(unix))]
    for e in walkdir_files(dir) {
        if let Ok(m) = std::fs::metadata(&e) {
            let mut p = m.permissions();
            #[allow(clippy::permissions_set_readonly_false)]
            p.set_readonly(false);
            let _ = std::fs::set_permissions(&e, p);
        }
    }
}

#[cfg(not(unix))]
fn walkdir_files(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let _ = walk(dir, Path::new(""), &mut out);
    out.into_iter().map(|r| dir.join(r)).collect()
}

/// An update cut short (av killed, the power gone) leaves `.<id>.old`:
/// finished when the new install is in, undone when it isn't.
fn recover(dest: &Path) -> Result<()> {
    let old = sibling(dest, "old");
    if !old.exists() {
        return Ok(());
    }
    log::warn!("finishing an update that was cut short: {}", old.display());
    if dest.exists() { finish_swap(&old, dest) } else { Ok(std::fs::rename(&old, dest)?) }
}

/// Move every file or folder under `old` that has no counterpart under `new`
/// into `new`, keeping its relative path. Folders present in both are merged
/// the same way; files present in both keep the new archive's copy. Both sit
/// in the same parent folder, so these are renames, not copies. With the
/// old install's list of shipped files, only the title's own files move;
/// without one (an install from before the list), everything the new
/// archive lacks does: a stray file is harmless, a lost save is not.
fn carry_over(old: &Path, new: &Path, shipped: Option<&HashSet<PathBuf>>) -> Result<()> {
    carry(old, new, Path::new(""), shipped)
}

fn carry(old: &Path, new: &Path, rel: &Path, shipped: Option<&HashSet<PathBuf>>) -> Result<()> {
    for entry in std::fs::read_dir(old.join(rel))? {
        let entry = entry?;
        let r = rel.join(entry.file_name());
        if r == Path::new(SHIPPED) || shipped.is_some_and(|s| s.contains(&r)) {
            continue;
        }
        let (from, to) = (old.join(&r), new.join(&r));
        let to_meta = std::fs::symlink_metadata(&to);
        if entry.file_type()?.is_dir() && (to_meta.as_ref().is_ok_and(|m| m.is_dir()) || shipped.is_some()) {
            // with a list, a folder may hold both kinds: go through it
            std::fs::create_dir_all(&to)?;
            carry(old, new, &r, shipped)?;
        } else if to_meta.is_err() {
            std::fs::create_dir_all(to.parent().unwrap_or(new))?;
            std::fs::rename(&from, &to)?;
        }
    }
    Ok(())
}

/// The binary may sit at the top of the archive or inside one directory.
/// It must be a real file: a symlink in an archive could point anywhere.
pub fn find_exec(dir: &Path, name: &str) -> Option<PathBuf> {
    let file = |p: &Path| std::fs::symlink_metadata(p).is_ok_and(|m| m.is_file());
    let top = dir.join(name);
    if file(&top) {
        return Some(top);
    }
    std::fs::read_dir(dir).ok()?.flatten().filter(|e| e.file_type().is_ok_and(|t| t.is_dir())).map(|e| e.path().join(name)).find(|p| file(p))
}

/// Set the executable bit explicitly instead of trusting the archive.
fn set_executable(path: &Path) -> Result<()> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut p = std::fs::metadata(path)?.permissions();
        p.set_mode(p.mode() | 0o755);
        std::fs::set_permissions(path, p)?;
    }
    let _ = path;
    Ok(())
}

/// The first lines of the scripts av writes into ~/.local/bin, by which it
/// knows them as its own.
const SCRIPT_MARK: &str = "#!/bin/sh\n# av: ";

/// Whether av may replace `l` in ~/.local/bin: a script of av's, or a
/// symlink into av's install folders (how av linked titles before).
#[cfg(unix)]
fn replaceable(l: &Path, dest: &Path) -> bool {
    let root = dest.parent().and_then(Path::parent).unwrap_or(dest);
    match std::fs::read_link(l) {
        Ok(target) => target.starts_with(root),
        Err(_) => std::fs::read_to_string(l).is_ok_and(|s| s.starts_with(SCRIPT_MARK)),
    }
}

/// Whether `l` starts the install under `dest`, so uninstalling it goes too.
fn starts(l: &Path, dest: &Path) -> bool {
    match std::fs::read_link(l) {
        Ok(target) => target.starts_with(dest),
        Err(_) => std::fs::read_to_string(l).is_ok_and(|s| s.starts_with(SCRIPT_MARK) && s.contains(&sh_quote(&dest.to_string_lossy()))),
    }
}

/// A word for sh, single-quoted.
fn sh_quote(s: &str) -> String {
    format!("'{}'", s.replace('\'', "'\\''"))
}

/// On Linux and macOS, `~/.local/bin/<id>` starts the title the way av
/// does: from its install folder, with the catalog's arguments, then
/// whatever was typed after the name. A file of the same name that isn't
/// av's (a pipx link, the player's own) is left alone, and the note says so.
fn link(bin: &Path, t: &Title, exe: &Path, dest: &Path) -> Result<Option<String>> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::create_dir_all(bin)?;
        let l = bin.join(&t.id);
        if l.symlink_metadata().is_ok() && !replaceable(&l, dest) {
            log::warn!("{} is not av's; leaving it", l.display());
            return Ok(Some(format!("{} is someone else's, so start {} from av", crate::paths::tilde(&l), t.id)));
        }
        let args: Vec<String> = t.exec.args.iter().map(|a| sh_quote(a)).collect();
        let script = format!(
            "{SCRIPT_MARK}starts {} as av does; av rewrites or removes this file\ncd {} || exit 1\nexec {}{}{} \"$@\"\n",
            t.id,
            sh_quote(&dest.to_string_lossy()),
            sh_quote(&exe.to_string_lossy()),
            if args.is_empty() { "" } else { " " },
            args.join(" ")
        );
        let tmp = bin.join(format!(".{}.av-tmp", t.id));
        std::fs::write(&tmp, script)?;
        std::fs::set_permissions(&tmp, std::fs::Permissions::from_mode(0o755))?;
        // a rename replaces an old link of av's in one step
        std::fs::rename(&tmp, &l)?;
    }
    let _ = (bin, t, exe, dest);
    Ok(None)
}

/// Remove a title. With `keep`, the files it wrote itself in its install
/// folder (saves, scores, settings) move there first, for a reinstall to
/// put back; without, they go too, and so do any kept from before.
pub fn uninstall(dest: &Path, bin: Option<&Path>, id: &str, saves: &Path, keep: bool) -> Result<()> {
    if let Some(bin) = bin {
        let l = bin.join(id);
        if l.symlink_metadata().is_ok() && starts(&l, dest) {
            std::fs::remove_file(&l)?;
        }
    }
    recover(dest)?;
    if dest.exists() {
        if keep {
            std::fs::create_dir_all(saves)?;
            carry_over(dest, saves, shipped(dest).as_ref()).context("keep the title's own files")?;
        }
        remove_all(dest)?;
    }
    if !keep {
        let _ = remove_all(saves);
    }
    // an empty folder means nothing of the title's own was there
    let _ = std::fs::remove_dir(saves);
    Ok(())
}

fn hex(b: &[u8]) -> String {
    b.iter().map(|x| format!("{x:02x}")).collect()
}

#[cfg(test)]
mod tests {
    use super::{Job, carry_over, extract, find_exec, recover, run, sibling, uninstall};
    use crate::testutil::serve;
    use crate::catalog::{self, Title};
    use std::collections::BTreeMap;
    use std::fs;
    use std::io::Write;
    use std::path::{Path, PathBuf};
    use std::sync::mpsc::channel;

    /// The binary's name in an archive, as `Title::exec_name` looks for it.
    const EXE: &str = std::env::consts::EXE_SUFFIX;

    /// A release archive as the catalog expects: the binary one folder down,
    /// executable, beside a data file.
    fn tar_gz(binary: &str, data: &str) -> Vec<u8> {
        tar_gz_with(&[(&format!("game/game{EXE}"), binary), ("game/data.txt", data)])
    }

    fn tar_gz_with(files: &[(&str, &str)]) -> Vec<u8> {
        let mut out = tar::Builder::new(flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::fast()));
        for (path, body) in files {
            let mut h = tar::Header::new_gnu();
            h.set_size(body.len() as u64);
            h.set_mode(if path.contains("data") || path.contains(".txt") { 0o644 } else { 0o755 });
            h.set_cksum();
            out.append_data(&mut h, path, body.as_bytes()).unwrap();
        }
        out.into_inner().unwrap().finish().unwrap()
    }

    fn sha(b: &[u8]) -> String {
        use sha2::{Digest, Sha256};
        super::hex(&Sha256::digest(b))
    }

    /// A catalog title for this machine, served from `url`, with `sha256`.
    fn title(version: &str, url: &str, sha256: &str) -> Title {
        title_with(version, url, sha256, r#""kind": "game""#)
    }

    /// The same with other fields, e.g. `"kind": "art"` or an `exec`.
    fn title_with(version: &str, url: &str, sha256: &str, fields: &str) -> Title {
        let target = catalog::host_targets()[0];
        let platform = format!(r#"{{"url": "{url}", "sha256": "{sha256}", "size": 0}}"#);
        let json = format!(r#"{{"id": "game", {fields}, "version": "{version}", "platforms": {{"{target}": {platform}}}}}"#);
        serde_json::from_str(&json).unwrap()
    }

    struct Home {
        root: PathBuf,
    }

    impl Home {
        fn new(name: &str) -> Home {
            let root = std::env::temp_dir().join(format!("av-{name}-{}", std::process::id()));
            let _ = fs::remove_dir_all(&root);
            fs::create_dir_all(&root).unwrap();
            Home { root }
        }
        fn dest(&self) -> PathBuf {
            self.root.join("games/game")
        }
        fn bin(&self) -> PathBuf {
            self.root.join("bin")
        }
        fn saves(&self) -> PathBuf {
            self.root.join("saves/game")
        }
        fn exe(&self) -> PathBuf {
            self.dest().join(format!("game/game{EXE}"))
        }
        fn job(&self, title: Title, catalog_url: &str) -> Job {
            Job {
                title,
                catalog_url: catalog_url.into(),
                cache: self.root.join("cache"),
                dest: self.dest(),
                saves: self.saves(),
                bin: Some(self.bin()),
                previous: None,
                cancel: None,
                job: 0,
            }
        }
    }

    impl Drop for Home {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.root);
        }
    }

    fn read(p: &Path) -> String {
        fs::read_to_string(p).unwrap()
    }

    #[test]
    fn install_update_and_uninstall_from_a_catalog() {
        let (v1, v2) = (tar_gz("binary 1", "data 1"), tar_gz("binary 2", "data 2"));
        let (sha1, sha2) = (sha(&v1), sha(&v2));
        let base = serve(BTreeMap::from([("/r/game-1.tar.gz".to_string(), v1), ("/r/game-2.tar.gz".to_string(), v2)]));
        let catalog_url = format!("{base}/data/catalog.json");
        let home = Home::new("install");
        let (tx, rx) = channel();

        // install: the archive's url is relative to the catalog's host, as on the website
        let done = run(&home.job(title("1.0", "/r/game-1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
        assert_eq!((done.version.as_str(), done.sha256.as_str()), ("1.0", sha1.as_str()));
        let exe = home.exe();
        assert_eq!(read(&exe), "binary 1");
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            assert_ne!(fs::metadata(&exe).unwrap().permissions().mode() & 0o111, 0, "the binary is executable");
            assert!(read(&home.bin().join("game")).contains(&*exe.to_string_lossy()), "~/.local/bin/game starts it");
        }
        assert!(rx.try_iter().any(|p| matches!(p, super::Progress::Status { msg, .. } if msg.starts_with("checksum ok"))));
        assert_eq!(fs::read_dir(home.root.join("cache/downloads")).unwrap().count(), 0, "the archive is removed after installing");

        // the title writes a save next to itself; an update keeps it
        fs::write(home.dest().join("game/save.dat"), "my save").unwrap();
        let done = run(&home.job(title("2.0", &format!("{base}/r/game-2.tar.gz"), &sha2), &catalog_url), &tx).unwrap();
        assert_eq!(done.version, "2.0");
        assert_eq!(read(&exe), "binary 2");
        assert_eq!(read(&home.dest().join("game/data.txt")), "data 2");
        assert_eq!(read(&home.dest().join("game/save.dat")), "my save");

        uninstall(&home.dest(), Some(&home.bin()), "game", &home.saves(), false).unwrap();
        assert!(!home.dest().exists());
        assert!(!home.saves().exists(), "remove takes the title's own files too");
        assert!(home.bin().join("game").symlink_metadata().is_err(), "the link is gone");
    }

    #[test]
    fn an_archive_url_with_a_query_or_no_extension_installs() {
        let v1 = tar_gz("binary 1", "data 1");
        let sha1 = sha(&v1);
        let base = serve(BTreeMap::from([("/files/game-1.0/download".to_string(), v1.clone()), ("/r/game.tar.gz?token=abc".to_string(), v1)]));
        let catalog_url = format!("{base}/catalog.json");
        let (tx, _rx) = channel();
        for url in ["/files/game-1.0/download", "/r/game.tar.gz?token=abc"] {
            let home = Home::new("url-shape");
            run(&home.job(title("1.0", url, &sha1), &catalog_url), &tx).unwrap_or_else(|e| panic!("{url}: {e:#}"));
            assert_eq!(read(&home.exe()), "binary 1");
        }
    }

    #[test]
    fn a_failed_install_leaves_no_download_or_staging_behind() {
        // checksum ok, but the binary the catalog names isn't in the archive
        let v1 = tar_gz_with(&[("other/thing", "x")]);
        let sha1 = sha(&v1);
        let base = serve(BTreeMap::from([("/r/1.tar.gz".to_string(), v1)]));
        let home = Home::new("leftovers");
        let (tx, _rx) = channel();
        assert!(run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &format!("{base}/catalog.json")), &tx).is_err());
        assert_eq!(fs::read_dir(home.root.join("cache/downloads")).unwrap().count(), 0);
        assert!(!sibling(&home.dest(), "new").exists());
    }

    /// From the shell, a title starts as it does from av: in its install
    /// folder, with the catalog's arguments, then what was typed.
    #[cfg(unix)]
    #[test]
    fn the_shell_starts_a_title_the_way_av_does() {
        let v1 = tar_gz_with(&[("game/game", "#!/bin/sh\necho \"$(pwd)|$*\"\n")]);
        let sha1 = sha(&v1);
        let base = serve(BTreeMap::from([("/r/1.tar.gz".to_string(), v1)]));
        let home = Home::new("script");
        let (tx, _rx) = channel();
        let t = title_with("1.0", "/r/1.tar.gz", &sha1, r#""kind": "game", "exec": {"game": "game", "args": ["-d", "it's here"]}"#);
        run(&home.job(t, &format!("{base}/catalog.json")), &tx).unwrap();
        let out = std::process::Command::new(home.bin().join("game")).arg("more").output().unwrap();
        let dest = fs::canonicalize(home.dest()).unwrap();
        // the shell's pwd may name the folder another way (macOS: /var is /private/var)
        let out = String::from_utf8_lossy(&out.stdout).trim().to_string();
        let (dir, rest) = out.split_once('|').expect("pwd|args");
        assert_eq!(fs::canonicalize(dir).unwrap(), dest);
        assert_eq!(rest, "-d it's here more");
        uninstall(&home.dest(), Some(&home.bin()), "game", &home.saves(), true).unwrap();
        assert!(home.bin().join("game").symlink_metadata().is_err(), "uninstall takes the script away");
    }

    /// Only what av made in ~/.local/bin is replaced or removed.
    #[cfg(unix)]
    #[test]
    fn someone_elses_file_in_local_bin_is_left_alone() {
        let v1 = tar_gz("binary 1", "data 1");
        let sha1 = sha(&v1);
        let base = serve(BTreeMap::from([("/r/1.tar.gz".to_string(), v1)]));
        let catalog_url = format!("{base}/catalog.json");
        let (tx, rx) = channel();
        let other = std::env::temp_dir().join(format!("av-pipx-tool-{}", std::process::id()));
        fs::write(&other, "pipx's").unwrap();
        for theirs in ["link", "file"] {
            let home = Home::new(&format!("theirs-{theirs}"));
            fs::create_dir_all(home.bin()).unwrap();
            let l = home.bin().join("game");
            if theirs == "link" { std::os::unix::fs::symlink(&other, &l).unwrap() } else { fs::write(&l, "my own script").unwrap() }
            run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
            assert!(rx.try_iter().any(|p| matches!(p, super::Progress::Status { msg, .. } if msg.contains("someone else's"))), "{theirs}: says so");
            uninstall(&home.dest(), Some(&home.bin()), "game", &home.saves(), true).unwrap();
            match theirs {
                "link" => assert_eq!(fs::read_link(&l).unwrap(), other),
                _ => assert_eq!(read(&l), "my own script"),
            }
        }
        // a symlink av made before it wrote scripts is av's to replace
        let home = Home::new("old-link");
        run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
        let l = home.bin().join("game");
        fs::remove_file(&l).unwrap();
        std::os::unix::fs::symlink(home.exe(), &l).unwrap();
        run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
        assert!(fs::read_link(&l).is_err() && read(&l).starts_with(super::SCRIPT_MARK));
        let _ = fs::remove_file(&other);
    }

    #[test]
    fn a_title_moved_to_another_kind_takes_its_install_along() {
        let v1 = tar_gz("binary 1", "data 1");
        let sha1 = sha(&v1);
        let base = serve(BTreeMap::from([("/r/1.tar.gz".to_string(), v1)]));
        let catalog_url = format!("{base}/catalog.json");
        let home = Home::new("kind");
        let (tx, _rx) = channel();
        run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
        fs::write(home.dest().join("game/save.dat"), "my save").unwrap();
        // the catalog now calls it art: the update goes to arts/, from games/
        let mut job = home.job(title_with("1.0", "/r/1.tar.gz", &sha1, r#""kind": "art""#), &catalog_url);
        job.previous = Some(home.dest());
        job.dest = home.root.join("arts/game");
        run(&job, &tx).unwrap();
        assert_eq!(read(&home.root.join("arts/game/game/save.dat")), "my save");
        assert!(!home.dest().exists());
    }

    #[test]
    fn uninstall_keeps_the_titles_own_files_for_a_reinstall() {
        let v1 = tar_gz("binary 1", "data 1");
        let sha1 = sha(&v1);
        let base = serve(BTreeMap::from([("/r/game-1.tar.gz".to_string(), v1)]));
        let catalog_url = format!("{base}/catalog.json");
        let home = Home::new("keep");
        let (tx, _rx) = channel();
        run(&home.job(title("1.0", "/r/game-1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
        fs::write(home.dest().join("game/empire.sav"), "a save").unwrap();
        fs::create_dir_all(home.dest().join("game/nki")).unwrap();
        fs::write(home.dest().join("game/nki/custom.nki"), "player's own").unwrap();

        uninstall(&home.dest(), Some(&home.bin()), "game", &home.saves(), true).unwrap();
        assert!(!home.dest().exists());
        assert_eq!(read(&home.saves().join("game/empire.sav")), "a save");
        assert_eq!(read(&home.saves().join("game/nki/custom.nki")), "player's own");
        assert!(!home.saves().join("game/data.txt").exists(), "the archive's own files are not kept");
        assert!(!home.saves().join(format!("game/game{EXE}")).exists());

        run(&home.job(title("1.0", "/r/game-1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
        assert_eq!(read(&home.dest().join("game/empire.sav")), "a save", "a reinstall puts them back");
        assert_eq!(read(&home.dest().join("game/nki/custom.nki")), "player's own");
        assert!(!home.saves().exists());
    }

    #[test]
    fn an_update_drops_files_the_old_release_shipped_but_keeps_saves() {
        let v1 = tar_gz_with(&[(&format!("game/game{EXE}"), "binary 1"), ("game/data.txt", "data 1"), ("game/old.txt", "gone in 2")]);
        let v2 = tar_gz("binary 2", "data 2");
        let (sha1, sha2) = (sha(&v1), sha(&v2));
        let base = serve(BTreeMap::from([("/r/1.tar.gz".to_string(), v1), ("/r/2.tar.gz".to_string(), v2)]));
        let catalog_url = format!("{base}/catalog.json");
        let home = Home::new("dropped");
        let (tx, _rx) = channel();
        run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
        fs::write(home.dest().join("game/save.dat"), "my save").unwrap();
        run(&home.job(title("2.0", "/r/2.tar.gz", &sha2), &catalog_url), &tx).unwrap();
        assert_eq!(read(&home.dest().join("game/save.dat")), "my save");
        assert!(!home.dest().join("game/old.txt").exists(), "a file 1.0 shipped and 2.0 doesn't is not carried");
        assert!(!sibling(&home.dest(), "old").exists());
    }

    #[test]
    fn an_update_cut_short_is_finished_by_the_next_one() {
        let v1 = tar_gz("binary 1", "data 1");
        let sha1 = sha(&v1);
        let base = serve(BTreeMap::from([("/r/1.tar.gz".to_string(), v1)]));
        let catalog_url = format!("{base}/catalog.json");
        let home = Home::new("cut-short");
        let (tx, _rx) = channel();
        run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
        fs::write(home.dest().join("game/save.dat"), "my save").unwrap();

        // killed right after moving the old install aside: nothing in place
        fs::rename(home.dest(), sibling(&home.dest(), "old")).unwrap();
        recover(&home.dest()).unwrap();
        assert_eq!(read(&home.dest().join("game/save.dat")), "my save", "undone: the old install is back");

        // killed after the new one went in, before the saves moved across
        let old = sibling(&home.dest(), "old");
        fs::rename(home.dest(), &old).unwrap();
        run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &catalog_url), &tx).unwrap_or_else(|e| panic!("{e:#}"));
        assert_eq!(read(&home.dest().join("game/save.dat")), "my save");
        assert!(!old.exists());
    }

    #[cfg(unix)]
    #[test]
    fn an_update_that_cannot_change_the_install_folder_leaves_it_alone() {
        use std::os::unix::fs::PermissionsExt;
        let v1 = tar_gz("binary 1", "data 1");
        let sha1 = sha(&v1);
        let base = serve(BTreeMap::from([("/r/1.tar.gz".to_string(), v1)]));
        let catalog_url = format!("{base}/catalog.json");
        let home = Home::new("locked");
        let (tx, _rx) = channel();
        run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
        fs::write(home.dest().join("game/save.dat"), "my save").unwrap();
        // the games folder can't be changed, as a locked file would on Windows
        let games = home.root.join("games");
        fs::set_permissions(&games, fs::Permissions::from_mode(0o555)).unwrap();
        let r = run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &catalog_url), &tx);
        fs::set_permissions(&games, fs::Permissions::from_mode(0o755)).unwrap();
        assert!(r.is_err());
        assert_eq!(read(&home.dest().join("game/save.dat")), "my save");
        assert_eq!(read(&home.exe()), "binary 1");
        // and the next try works
        run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
        assert_eq!(read(&home.dest().join("game/save.dat")), "my save");
    }

    /// The review's case: an archive with a read-only folder made deleting
    /// the old install fail half way, and the next update deleted the saves.
    #[cfg(unix)]
    #[test]
    fn an_archive_with_a_read_only_folder_updates_and_keeps_saves() {
        let mut b = tar::Builder::new(flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::fast()));
        for (path, body, mode, dir) in [("game/", "", 0o555, true), (&*format!("game/game{EXE}"), "binary", 0o755, false), ("game/ro/", "", 0o555, true), ("game/ro/f.txt", "x", 0o444, false)] {
            let mut h = tar::Header::new_gnu();
            h.set_entry_type(if dir { tar::EntryType::Directory } else { tar::EntryType::Regular });
            h.set_size(body.len() as u64);
            h.set_mode(mode);
            h.set_cksum();
            b.append_data(&mut h, path, body.as_bytes()).unwrap();
        }
        let archive = b.into_inner().unwrap().finish().unwrap();
        let sha1 = sha(&archive);
        let base = serve(BTreeMap::from([("/r/1.tar.gz".to_string(), archive)]));
        let catalog_url = format!("{base}/catalog.json");
        let home = Home::new("read-only");
        let (tx, _rx) = channel();
        run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
        // the title writes its save beside itself (its folder is read-only, so next to it)
        fs::write(home.dest().join("save.dat"), "my save").unwrap();
        for _ in 0..2 {
            run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &catalog_url), &tx).unwrap_or_else(|e| panic!("{e:#}"));
            assert_eq!(read(&home.dest().join("save.dat")), "my save");
        }
        assert!(!sibling(&home.dest(), "old").exists(), "the old install was removed despite its read-only folders");
        uninstall(&home.dest(), Some(&home.bin()), "game", &home.saves(), false).unwrap();
        assert!(!home.dest().exists());
    }

    #[cfg(unix)]
    #[test]
    fn a_binary_that_is_a_symlink_is_refused() {
        use std::os::unix::fs::PermissionsExt;
        let home = Home::new("symlink");
        let secret = home.root.join("secret");
        fs::write(&secret, "private").unwrap();
        fs::set_permissions(&secret, fs::Permissions::from_mode(0o600)).unwrap();
        let mut b = tar::Builder::new(flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::fast()));
        let mut h = tar::Header::new_gnu();
        h.set_entry_type(tar::EntryType::Symlink);
        h.set_size(0);
        h.set_cksum();
        b.append_link(&mut h, "game/game", &secret).unwrap();
        let archive = b.into_inner().unwrap().finish().unwrap();
        let sha1 = sha(&archive);
        let base = serve(BTreeMap::from([("/r/1.tar.gz".to_string(), archive)]));
        let (tx, _rx) = channel();
        let err = run(&home.job(title("1.0", "/r/1.tar.gz", &sha1), &format!("{base}/catalog.json")), &tx).unwrap_err();
        assert!(err.to_string().contains("not found in the archive"), "{err:#}");
        assert_eq!(fs::metadata(&secret).unwrap().permissions().mode() & 0o777, 0o600, "the target was not touched");
        assert!(home.bin().join("game").symlink_metadata().is_err());
    }

    #[test]
    fn a_download_that_does_not_match_its_checksum_is_not_installed() {
        let archive = tar_gz("binary", "data");
        let base = serve(BTreeMap::from([("/r/game.tar.gz".to_string(), archive)]));
        let home = Home::new("checksum");
        let (tx, _rx) = channel();
        let wrong = "0".repeat(64);
        let err = run(&home.job(title("1.0", "/r/game.tar.gz", &wrong), &format!("{base}/catalog.json")), &tx).unwrap_err();
        assert!(err.to_string().starts_with("checksum mismatch"), "{err:#}");
        assert!(!home.dest().exists(), "nothing is extracted");
        assert_eq!(fs::read_dir(home.root.join("cache/downloads")).unwrap().count(), 0, "the bad download is deleted");
        assert!(home.bin().join("game").symlink_metadata().is_err());
    }

    #[test]
    fn a_failed_update_leaves_the_old_install_alone() {
        let good = tar_gz("binary 1", "data 1");
        let sha1 = sha(&good);
        let base = serve(BTreeMap::from([("/r/game-1.tar.gz".to_string(), good)]));
        let catalog_url = format!("{base}/catalog.json");
        let home = Home::new("failed-update");
        let (tx, _rx) = channel();
        run(&home.job(title("1.0", "/r/game-1.tar.gz", &sha1), &catalog_url), &tx).unwrap();
        // version 2's archive is missing from the server
        assert!(run(&home.job(title("2.0", "/r/game-2.tar.gz", &sha1), &catalog_url), &tx).is_err());
        assert_eq!(read(&home.exe()), "binary 1");
    }

    #[test]
    fn zip_extracts_with_the_binary_one_folder_down() {
        let root = std::env::temp_dir().join(format!("av-zip-{}", std::process::id()));
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(&root).unwrap();
        let archive = root.join("game-x86_64-pc-windows-msvc.zip");
        let mut z = zip::ZipWriter::new(fs::File::create(&archive).unwrap());
        let opts = zip::write::SimpleFileOptions::default().compression_method(zip::CompressionMethod::Deflated);
        z.add_directory("game/data/", opts).unwrap();
        z.start_file("game/game.exe", opts).unwrap();
        z.write_all(b"binary").unwrap();
        z.start_file("game/data/levels.txt", opts).unwrap();
        z.write_all(b"level 1").unwrap();
        z.finish().unwrap();

        let dest = root.join("out");
        extract(&archive, &dest).unwrap();
        assert_eq!(find_exec(&dest, "game.exe"), Some(dest.join("game/game.exe")));
        assert_eq!(fs::read_to_string(dest.join("game/data/levels.txt")).unwrap(), "level 1");
        let not_an_archive = root.join("game.7z");
        fs::write(&not_an_archive, b"7z\xbc\xaf").unwrap();
        assert!(extract(&not_an_archive, &dest).is_err());
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn update_keeps_files_the_archive_does_not_have() {
        let root = std::env::temp_dir().join(format!("av-carry-over-{}", std::process::id()));
        let _ = fs::remove_dir_all(&root);
        let (old, new) = (root.join("old"), root.join("new"));
        fs::create_dir_all(old.join("nki")).unwrap();
        fs::create_dir_all(old.join("saves")).unwrap();
        fs::write(old.join("game"), "old binary").unwrap();
        fs::write(old.join("empire.sav"), "a save").unwrap();
        fs::write(old.join("nki/vanilla.nki"), "old data").unwrap();
        fs::write(old.join("nki/custom.nki"), "player's own").unwrap();
        fs::write(old.join("saves/slot1"), "slot").unwrap();
        fs::create_dir_all(new.join("nki")).unwrap();
        fs::write(new.join("game"), "new binary").unwrap();
        fs::write(new.join("nki/vanilla.nki"), "new data").unwrap();

        carry_over(&old, &new, None).unwrap();

        let read = |p: &str| fs::read_to_string(new.join(p)).unwrap();
        assert_eq!(read("game"), "new binary");
        assert_eq!(read("nki/vanilla.nki"), "new data");
        assert_eq!(read("empire.sav"), "a save");
        assert_eq!(read("nki/custom.nki"), "player's own");
        assert_eq!(read("saves/slot1"), "slot");
        let _ = fs::remove_dir_all(&root);
    }
}
