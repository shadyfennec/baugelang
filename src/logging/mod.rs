//! Utilities for logging debug data.

use std::{
    cell::RefCell,
    collections::HashMap,
    io,
    path::{Path, PathBuf},
    sync::{
        OnceLock,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
    time::{Duration, Instant},
};

thread_local! {
    static LOGS: RefCell<HashMap<&'static str, Log>> = RefCell::new(HashMap::new());
}

static ENABLE: AtomicBool = const { AtomicBool::new(false) };
static COUNTER: AtomicU64 = const { AtomicU64::new(0) };
static PATH: OnceLock<PathBuf> = OnceLock::new();

/// Measures the amount of time it takes to execute the function passed as a
/// parameter.
pub fn time<F, R>(f: F) -> (R, Duration)
where
    F: FnOnce() -> R,
{
    let start = Instant::now();
    let r = f();
    (r, start.elapsed())
}

/// Formats time for printing purposes.
pub fn format_time(t: &Duration) -> String {
    let nanos = t.as_nanos();

    if nanos == 0 {
        return "0ns".to_string();
    }

    let log10 = nanos.ilog10();

    if log10 < 3 {
        format!("{}ns", nanos)
    } else if log10 < 6 {
        format!("{}µs", nanos / 1000)
    } else if log10 < 9 {
        format!("{}ms", nanos / 1_000_000)
    } else {
        format!("{}s", nanos / 1_000_000_000)
    }
}

/// Enables logging.
pub fn enable() {
    ENABLE.store(true, Ordering::SeqCst);
}

fn path() -> &'static Path {
    PATH.get_or_init(|| {
        let path = PathBuf::from("./logs");
        if !std::fs::exists(&path).unwrap() {
            std::fs::create_dir(&path).unwrap();
        }
        path
    })
}

/// A structured Markdown log for debugging purposes.
#[derive(Default)]
pub struct Log {
    pub inner: String,
}

pub fn with_global_log<F>(name: &'static str, f: F)
where
    F: FnOnce(&mut Log),
{
    if ENABLE.load(Ordering::SeqCst) {
        LOGS.with_borrow_mut(|h| f(h.entry(name).or_default()))
    }
}

#[macro_export]
macro_rules! log {
    ($n:literal, h1, $($args:expr),*) => {{
        $crate::logging::with_global_log($n, |l| {
            l.inner
                .push_str(&::std::format!("# {}\n", ::std::format!($($args),*)))
        })
    }};
    ($n:literal, h2, $($args:expr),*) => {{
        $crate::logging::with_global_log($n, |l| {
            l.inner
                .push_str(&::std::format!("## {}\n", ::std::format!($($args),*)))
        })
    }};
    ($n:literal, h3, $($args:expr),*) => {{
        $crate::logging::with_global_log($n, |l| {
            l.inner
                .push_str(&::std::format!("### {}\n", ::std::format!($($args),*)))
        })
    }};
    ($n:literal, p, $($args:expr),*) => {{
        $crate::logging::with_global_log($n, |l| {
            l.inner
                .push_str(&::std::format!("{}\n\n", ::std::format!($($args),*)))
        })
    }};
    ($n:literal, code, $($args:expr),*) => {{
        $crate::logging::with_global_log($n, |l| {
            l.inner
                .push_str(&::std::format!("```\n{}\n```\n", ::std::format!($($args),*)))
        })
    }};
    ($n:literal, err, $($args:expr),*) => {{
        $crate::logging::with_global_log($n, |l| {
            let s = ::std::format!($($args),*);
            l.inner.push_str("> ❌ Error\n");
            for line in s.lines() {
                l.inner.push_str(&::std::format!("> {}\n", line))
            }
            l.inner.push('\n');
        })
    }};
    ($n:literal, finish) => {
        $crate::logging::finish($n).unwrap();
    }
}

/// Write the document to the corresponding file.
pub fn finish(name: &'static str) -> Result<(), io::Error> {
    LOGS.with_borrow_mut(|h| {
        if let Some(log) = h.remove(name) {
            let counter = COUNTER.fetch_add(1, Ordering::SeqCst);
            let mut path = path().to_path_buf();
            path.push(PathBuf::from(format!("{counter}_{name}.md")));

            std::fs::write(path, &log.inner)
        } else {
            Ok(())
        }
    })
}
