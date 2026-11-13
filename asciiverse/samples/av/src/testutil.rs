//! Helpers for tests only.

use std::collections::BTreeMap;
use std::io::{BufRead, BufReader, Write};
use std::net::TcpListener;

/// A tiny HTTP server on 127.0.0.1 that serves `files` by path, for as
/// long as the test runs. Returns its base URL.
pub fn serve(files: BTreeMap<String, Vec<u8>>) -> String {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let base = format!("http://{}", listener.local_addr().unwrap());
    std::thread::spawn(move || {
        for stream in listener.incoming() {
            let Ok(mut stream) = stream else { continue };
            let mut line = String::new();
            if BufReader::new(&stream).read_line(&mut line).is_err() {
                continue;
            }
            let path = line.split_whitespace().nth(1).unwrap_or("/").to_string();
            let head = |status: &str, len: usize| format!("HTTP/1.1 {status}\r\nContent-Length: {len}\r\nConnection: close\r\n\r\n");
            match files.get(&path) {
                Some(body) => {
                    let _ = stream.write_all(head("200 OK", body.len()).as_bytes());
                    let _ = stream.write_all(body);
                }
                None => {
                    let _ = stream.write_all(head("404 Not Found", 0).as_bytes());
                }
            }
        }
    });
    base
}
