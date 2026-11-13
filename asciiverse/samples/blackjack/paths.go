package main

import (
	"os"
	"path/filepath"
	"runtime"
)

// stateDir is where the game keeps the files it writes, save.txt and
// debug.log: $XDG_STATE_HOME/blackjack-tui, else the platform's usual place
// (~/.local/state/blackjack-tui, ~/Library/Application Support/blackjack-tui,
// %LocalAppData%\blackjack-tui). It is created on first use. If no home
// folder can be found it falls back to the current folder.
func stateDir() string {
	dir := "."
	if d := os.Getenv("XDG_STATE_HOME"); d != "" {
		dir = filepath.Join(d, "blackjack-tui")
	} else {
		switch runtime.GOOS {
		case "windows":
			if d := os.Getenv("LocalAppData"); d != "" {
				dir = filepath.Join(d, "blackjack-tui")
			}
		case "darwin":
			if d, err := os.UserConfigDir(); err == nil {
				dir = filepath.Join(d, "blackjack-tui")
			}
		default:
			if h, err := os.UserHomeDir(); err == nil {
				dir = filepath.Join(h, ".local", "state", "blackjack-tui")
			}
		}
	}
	_ = os.MkdirAll(dir, 0o755)
	return dir
}

// statePath is the full path of a file in stateDir.
func statePath(name string) string {
	return filepath.Join(stateDir(), name)
}
