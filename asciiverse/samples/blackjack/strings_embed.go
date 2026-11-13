package main

import (
	"bytes"
	_ "embed"
	"io"
	"os"
	"path/filepath"
)

//go:embed strings.json
var builtinStrings []byte

// openStrings opens the game's text: a strings.json beside the program
// when there is one, so the text can still be edited or translated, else
// the copy built into the program. The game then starts from any folder.
func openStrings(name string) (io.ReadCloser, error) {
	if exe, err := os.Executable(); err == nil {
		if f, err := os.Open(filepath.Join(filepath.Dir(exe), name)); err == nil {
			return f, nil
		}
	}
	return io.NopCloser(bytes.NewReader(builtinStrings)), nil
}
