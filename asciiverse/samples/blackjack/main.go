package main

import (
	"bytes"
	"fmt"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/rivo/uniseg"
	"log"
	"os"
	"runtime/debug"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"
)

// ------------------- Main -----------------------------------

func main() {
	debug.SetGCPercent(10)

	logFile := setupLogging(statePath(debugFile))
	defer func() {
		err := logFile.Close()
		if err != nil {
			fmt.Print("Error while closing " + debugFile + ": " + err.Error())
		}
	}()

	p := tea.NewProgram(
		languageModel{Cursor: 0, Page: 0},
		tea.WithFPS(120), tea.WithAltScreen(),
	)
	_, _ = p.Run()
}

func setupLogging(file string) *os.File {
	logFile, err := os.Create(file)
	if err != nil {
		fmt.Println("Error creating/reading log file: ")
		fmt.Println(err.Error())
		fmt.Println("Press Enter to continue without debug file")
		_, _ = fmt.Scanln()
	}

	log.SetOutput(logFile)
	log.SetFlags(log.Ltime | log.Lmicroseconds | log.Lshortfile)

	return logFile
}

// ------------------- General BJ Supporting Functions -------------------

func renderOptions(options []string, selected int) string {
	if len(options) == 0 {
		return emptyString
	}
	capacity := optionCursorPrefixLen + optionEmptyPrefixLen*(len(options)-1)
	for _, option := range options {
		capacity += len(option)
	}
	capacity += len(options) - 1

	var builder strings.Builder
	builder.Grow(capacity)

	for i, option := range options {
		if i == selected {
			builder.WriteString(optionCursorPrefix)
		} else {
			builder.WriteString(optionEmptyPrefix)
		}
		builder.WriteString(option)
		if i < len(options)-1 {
			builder.WriteByte(newlineRune)
		}
	}
	return builder.String()
}

func paginateLines(lines []string, page int) []string {
	total := len(lines)
	if total == 0 || page < 0 {
		return []string{}
	}

	firstPageSize := 5
	otherPageSize := 5

	if page == 0 {
		end := min(firstPageSize, total)
		if total > firstPageSize {
			result := make([]string, 0, end+2)
			result = append(result, threeHyphens)
			result = append(result, lines[:end]...)
			result = append(result, fourDots)
			return result
		} else {
			return lines[:end]
		}
	}

	start := firstPageSize + (page-1)*otherPageSize
	if start >= total {
		return []string{}
	}

	end := min(start+otherPageSize, total)
	result := make([]string, 0, end-start+2)
	result = append(result, threeDots)
	result = append(result, lines[start:end]...)
	if end < total {
		result = append(result, fourDots)
	}

	return result
}

func windowSize(currentWidth, currentHeight int, hideCursorPending bool, msg tea.WindowSizeMsg) (int, int, bool, tea.Cmd) {
	if msg.Width == 0 || msg.Height == 0 {
		return currentWidth, currentHeight, hideCursorPending, nil
	}
	if currentWidth == msg.Width && currentHeight == msg.Height {
		return currentWidth, currentHeight, hideCursorPending, nil
	}
	if !hideCursorPending {
		hideCursorPending = true
		return msg.Width, msg.Height, hideCursorPending, tea.Tick(500*time.Millisecond, func(t time.Time) tea.Msg { return customHideCursorMsg{} })
	} // immediately calling tea.HideCursor on WindowSizeMsg did not work
	return msg.Width, msg.Height, hideCursorPending, nil
}

// ------------------- Reusable / Generic Wrapping and Padding -----------

// TW = TabWidth
func wrapUnconditionalTW(text string, width int, tabWidth int) string {
	if width <= 0 {
		return emptyString
	}
	containsTab := strings.Contains(text, tabString)
	if !containsTab && len(text) <= width {
		return text
	}
	text = strings.ReplaceAll(text, tabString, generateSpaces(tabWidth))
	if len(text) <= width {
		return text
	}
	if isOnlyASCII(text) {
		return wrapASCIIString(text, width)
	} else {
		return wrapUnicodeString(text, width)
	}
}

func wrapUnconditionalTWToBuffer(dst []byte, text string, width int, tabWidth int) []byte {
	if width <= 0 {
		return dst
	}
	containsTab := strings.Contains(text, tabString)
	if !containsTab && len(text) <= width {
		return append(dst, text...)
	}
	text = strings.ReplaceAll(text, tabString, generateSpaces(tabWidth))
	if len(text) <= width {
		return append(dst, text...)
	}
	if isOnlyASCII(text) {
		return wrapASCIIByte(dst, text, width)
	} else {
		return wrapUnicodeByte(dst, text, width)
	}
}

func isOnlyASCII(s string) bool {
	// based on https://stackoverflow.com/a/77689444/16963475 - optimized - about 30 % faster
	for len(s) >= 8 {
		first32 := uint32(s[0]) | uint32(s[1])<<8 | uint32(s[2])<<16 | uint32(s[3])<<24
		second32 := uint32(s[4]) | uint32(s[5])<<8 | uint32(s[6])<<16 | uint32(s[7])<<24
		if (first32|second32)&0x80808080 != 0 {
			return false
		}
		s = s[8:]
	}
	for i := 0; i < len(s); i++ {
		if s[i] >= 128 {
			return false
		}
	}
	return true
}

func wrapASCIIString(text string, width int) string {
	var result strings.Builder
	result.Grow(len(text) + len(text)/width)
	currentWidth := 0
	for i := 0; i < len(text); i++ {
		c := text[i]
		if c == newlineRune {
			result.WriteByte(c)
			currentWidth = 0
			continue
		}
		if currentWidth+1 <= width {
			result.WriteByte(c)
			currentWidth++
			continue
		}
		result.WriteByte(newlineRune)
		currentWidth = 0
		j := i
		for j < len(text) && text[j] == singleSpaceRune {
			j++
		}
		if j >= len(text) {
			break
		}
		result.WriteByte(text[j])
		currentWidth = 1
		i = j
	}
	return result.String()
}

func wrapASCIIByte(dst []byte, text string, width int) []byte {
	currentWidth := 0
	for i := 0; i < len(text); i++ {
		c := text[i]
		if c == newlineRune {
			dst = append(dst, c)
			currentWidth = 0
			continue
		}
		if currentWidth+1 <= width {
			dst = append(dst, c)
			currentWidth++
			continue
		}
		dst = append(dst, newlineRune)
		currentWidth = 0
		j := i
		for j < len(text) && text[j] == singleSpaceRune {
			j++
		}
		if j >= len(text) {
			break
		}
		dst = append(dst, text[j])
		currentWidth = 1
		i = j
	}
	return dst
}

func wrapUnicodeString(text string, width int) string {
	var result strings.Builder
	result.Grow(len(text) + len(text)/width)
	currentWidth := 0
	for i := 0; i < len(text); {
		r, size := utf8.DecodeRuneInString(text[i:])
		if r == newlineRune {
			result.WriteRune(r)
			currentWidth = 0
			i += size
			continue
		}
		charWidth := uniseg.StringWidth(text[i : i+size])
		if currentWidth+charWidth <= width {
			result.WriteRune(r)
			currentWidth += charWidth
			i += size
			continue
		}
		result.WriteRune(newlineRune)
		currentWidth = 0
		i = skipSpaces(text, i)
	}
	return result.String()
}

func wrapUnicodeByte(dst []byte, text string, width int) []byte {
	currentWidth := 0
	for i := 0; i < len(text); {
		r, size := utf8.DecodeRuneInString(text[i:])
		if r == newlineRune {
			dst = utf8.AppendRune(dst, r)
			currentWidth = 0
			i += size
			continue
		}
		charWidth := uniseg.StringWidth(text[i : i+size])
		if currentWidth+charWidth <= width {
			dst = utf8.AppendRune(dst, r)
			currentWidth += charWidth
			i += size
			continue
		}
		dst = append(dst, newlineRune)
		currentWidth = 0
		i = skipSpaces(text, i)
	}
	return dst
}

func skipSpaces(text string, start int) int {
	for start < len(text) {
		if text[start] == byte(singleSpaceRune) {
			start++
			continue
		}
		r, size := utf8.DecodeRuneInString(text[start:])
		if r == newlineRune {
			break
		}
		if !unicode.IsSpace(r) {
			break
		}
		start += size
	}
	return start
}

// WM = WithMargin
func wrapAndPadWMToBuffer(dst []byte, s string, width int, margin int) []byte {
	if width <= 0 {
		return dst
	}
	contentWidth := width - margin
	if contentWidth <= 0 {
		return dst
	}
	wrapped := wrapUnconditionalTW(s, contentWidth, 4)
	lines := strings.Split(wrapped, newlineString)
	for i, line := range lines {
		lineWidth := uniseg.StringWidth(line)
		diff := width - lineWidth
		if diff <= 0 {
			log.Print("[WARN] Line exceeds width" + newlineString)
			line = emptyString
		}
		dst = append(dst, line...)
		if diff > 0 {
			dst = append(dst, generateSpaces(diff)...)
		}
		if i < len(lines)-1 {
			dst = append(dst, newlineRune)
		}
	}
	return dst
}

func generateSpaces(n int) string {
	switch {
	case n <= 0:
		return emptyString
	case n <= len(longSpace):
		return longSpace[:n]
	default:
		if n != cachedSpacesWidth {
			cachedSpacesString = strings.Repeat(singleSpaceString, n)
			cachedSpacesWidth = n
		}
		return cachedSpacesString
	}
}

func verticalPaddingToBuffer(buf []byte, width, height int) []byte {
	if width <= 0 || height <= 0 {
		return buf
	}
	if len(buf) == 0 {
		log.Print("[WARN] verticalPadding called empty" + newlineString)
		return buf
	}

	lineCount := bytes.Count(buf, []byte(newlineString)) + 1
	if lineCount > height {
		log.Print("[WARN] Content exceeds height" + newlineString)
		return buf
	}

	paddingLines := height - lineCount
	if paddingLines <= 0 {
		return buf
	}

	for i := 0; i < paddingLines; i++ {
		buf = append(buf, generateSpaces(width)...)
		buf = append(buf, newlineRune)
	}

	return buf
}
