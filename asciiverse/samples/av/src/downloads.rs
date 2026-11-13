//! The downloads queue behind the TUI's downloads pane: installs and
//! updates run one at a time, the rest wait their turn; a failed or
//! cancelled one can be retried, and finished ones stay listed until
//! cleared. The worker threads are in install.rs; this only keeps the books.

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Instant;

#[derive(Clone, Debug, PartialEq)]
pub enum Phase {
    Queued,
    Running,
    Done,
    Failed(String),
    Cancelled,
}

#[derive(Debug)]
pub struct Download {
    pub id: String,
    pub name: String,
    pub version: String,
    /// the installed version, for an update
    pub from: Option<String>,
    pub phase: Phase,
    pub done: u64,
    pub total: u64,
    /// the worker's current step: downloading, checksum ok, extracting
    pub step: String,
    pub started: Option<Instant>,
    /// set to stop the running download; the worker checks it per chunk
    pub cancel: Arc<AtomicBool>,
    /// which worker run this is, so late messages from a cancelled one are told apart
    pub job: u64,
}

impl Download {
    pub fn finished(&self) -> bool {
        !matches!(self.phase, Phase::Queued | Phase::Running)
    }

    /// Average speed since the download began, in bytes a second.
    pub fn speed(&self) -> f64 {
        let secs = self.started.map(|t| t.elapsed().as_secs_f64()).unwrap_or(0.0);
        if secs > 0.5 { self.done as f64 / secs } else { 0.0 }
    }
}

#[derive(Default)]
pub struct Downloads {
    pub list: Vec<Download>,
    jobs: u64,
}

impl Downloads {
    /// Queue an install or update; false when that title is already queued
    /// or running. A finished entry for the same title gives way to it.
    pub fn queue(&mut self, id: &str, name: &str, version: &str, from: Option<String>) -> bool {
        if self.busy(id) {
            return false;
        }
        self.list.retain(|d| d.id != id);
        self.list.push(Download {
            id: id.into(),
            name: name.into(),
            version: version.into(),
            from,
            phase: Phase::Queued,
            done: 0,
            total: 0,
            step: String::new(),
            started: None,
            cancel: Arc::new(AtomicBool::new(false)),
            job: 0,
        });
        true
    }

    pub fn busy(&self, id: &str) -> bool {
        self.list.iter().any(|d| d.id == id && !d.finished())
    }

    pub fn running(&self) -> Option<&Download> {
        self.list.iter().find(|d| d.phase == Phase::Running)
    }

    /// The running download that worker run `job` belongs to.
    pub fn running_mut(&mut self, job: u64) -> Option<&mut Download> {
        self.list.iter_mut().find(|d| d.job == job && d.phase == Phase::Running)
    }

    pub fn queued(&self) -> usize {
        self.list.iter().filter(|d| d.phase == Phase::Queued).count()
    }

    pub fn failed(&self) -> usize {
        self.list.iter().filter(|d| matches!(d.phase, Phase::Failed(_))).count()
    }

    /// When nothing runs, the first queued download becomes the running one;
    /// the caller starts its worker.
    pub fn start_next(&mut self) -> Option<&Download> {
        if self.running().is_some() {
            return None;
        }
        self.jobs += 1;
        let job = self.jobs;
        let d = self.list.iter_mut().find(|d| d.phase == Phase::Queued)?;
        d.phase = Phase::Running;
        d.job = job;
        d.cancel = Arc::new(AtomicBool::new(false));
        d.started = Some(Instant::now());
        d.step = "starting".into();
        Some(d)
    }

    /// The worker gave up. False when that download was already cancelled
    /// in the list (or is gone), so there is nothing to report.
    pub fn fail(&mut self, job: u64, error: String) -> bool {
        match self.running_mut(job) {
            Some(d) => {
                d.phase = Phase::Failed(error);
                true
            }
            None => false,
        }
    }

    /// The worker finished, also one cancelled too late to stop it: its
    /// files are in place, so the list says so.
    pub fn finish(&mut self, job: u64) {
        if let Some(d) = self.list.iter_mut().find(|d| d.job == job && matches!(d.phase, Phase::Running | Phase::Cancelled)) {
            d.phase = Phase::Done;
            d.done = d.total;
        }
    }

    /// The pane's order: running, queued in turn, failed, cancelled, done.
    pub fn ordered(&self) -> Vec<usize> {
        let rank = |p: &Phase| match p {
            Phase::Running => 0,
            Phase::Queued => 1,
            Phase::Failed(_) => 2,
            Phase::Cancelled => 3,
            Phase::Done => 4,
        };
        let mut v: Vec<usize> = (0..self.list.len()).collect();
        v.sort_by_key(|&i| rank(&self.list[i].phase));
        v
    }

    /// Del in the pane: stop a running download, drop a queued one, or take
    /// a finished one off the list.
    pub fn cancel_or_remove(&mut self, i: usize) {
        let Some(d) = self.list.get_mut(i) else { return };
        match d.phase {
            // cancelled at once, so the next one can start; the worker stops
            // at its next chunk, or when a stalled connection gives up
            Phase::Running => {
                d.cancel.store(true, Ordering::Relaxed);
                d.phase = Phase::Cancelled;
            }
            Phase::Queued => d.phase = Phase::Cancelled,
            _ => {
                self.list.remove(i);
            }
        }
    }

    /// Enter in the pane: a failed or cancelled download goes back in the queue.
    pub fn retry(&mut self, i: usize) -> bool {
        let Some(d) = self.list.get(i) else { return false };
        if !matches!(d.phase, Phase::Failed(_) | Phase::Cancelled) {
            return false;
        }
        let d = self.list.remove(i);
        self.queue(&d.id, &d.name, &d.version, d.from)
    }

    /// r in the pane: every failed download back in the queue, in order.
    pub fn retry_failed(&mut self) -> usize {
        let failed: Vec<usize> = (0..self.list.len()).rev().filter(|&i| matches!(self.list[i].phase, Phase::Failed(_))).collect();
        let mut back: Vec<Download> = failed.into_iter().map(|i| self.list.remove(i)).collect();
        back.reverse();
        let n = back.len();
        for d in back {
            self.queue(&d.id, &d.name, &d.version, d.from);
        }
        n
    }

    pub fn clear_finished(&mut self) {
        self.list.retain(|d| !d.finished());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_at_a_time_with_retry_and_cancel() {
        let mut q = Downloads::default();
        assert!(q.queue("a", "A", "1", None));
        assert!(q.queue("b", "B", "2", Some("1".into())));
        assert!(!q.queue("a", "A", "1", None), "already queued");
        let ja = q.start_next().map(|d| (d.id.clone(), d.job)).unwrap();
        assert_eq!(ja.0, "a");
        assert!(q.start_next().is_none(), "one runs at a time");

        assert!(q.fail(ja.1, "network down".into()));
        assert_eq!(q.list[0].phase, Phase::Failed("network down".into()));
        let jb = q.start_next().map(|d| d.job).unwrap();

        // cancelling the running one takes effect at once, and the next can start
        let b = q.ordered()[0];
        q.cancel_or_remove(b);
        assert_eq!(q.list.iter().find(|d| d.id == "b").unwrap().phase, Phase::Cancelled);
        assert!(q.running().is_none());
        // the worker's late "cancelled" changes nothing
        assert!(!q.fail(jb, "cancelled".into()));
        assert_eq!(q.list.iter().find(|d| d.id == "b").unwrap().phase, Phase::Cancelled);

        // r: failures go back in the queue, cancelled ones stay put
        assert_eq!(q.retry_failed(), 1);
        assert_eq!(q.list.iter().find(|d| d.id == "a").unwrap().phase, Phase::Queued);
        let ja2 = q.start_next().map(|d| d.job).unwrap();
        assert_ne!(ja2, ja.1, "a new run, a new job number");
        // a message from a's first, failed run doesn't touch the new one
        assert!(q.running_mut(ja.1).is_none());
        q.finish(ja2);

        // Enter: a cancelled one is retried too
        let b = q.list.iter().position(|d| d.id == "b").unwrap();
        assert!(q.retry(b));
        let jb2 = q.start_next().map(|d| d.job).unwrap();
        // the old, cancelled worker of b finishing late is not this run's result
        q.finish(jb);
        assert_eq!(q.running().map(|d| d.job), Some(jb2));
        q.finish(jb2);
        assert_eq!(q.failed(), 0);
        q.clear_finished();
        assert!(q.list.is_empty());
    }
}
