package main

import (
	"encoding/json"
	"fmt"
	tea "github.com/charmbracelet/bubbletea"
	"log"
	"sort"
)

// ------------------- Language Model -------------------------

func (m languageModel) Init() tea.Cmd {
	loadLanguages := func() tea.Msg {
		languages, err := loadLanguagesFromJSON(path)
		if err != nil {
			return customErrorMsg{err: err}
		}
		return customFinishMsg{languages: languages}
	}
	return tea.Batch(loadLanguages, tea.WindowSize())
}

func loadLanguagesFromJSON(filename string) ([]string, error) {
	file, err := openStrings(filename)
	if err != nil {
		log.Printf("failed to open file: %v", err)
		return nil, fmt.Errorf("failed to open file: %w", err)
	}
	defer func() {
		closeErr := file.Close()
		if closeErr != nil {
			log.Println("Error while closing " + filename + ": " + closeErr.Error())
		}
	}()

	var jsonData map[string]interface{}
	err = json.NewDecoder(file).Decode(&jsonData)
	if err != nil {
		log.Println("failed to decode " + filename)
		return nil, fmt.Errorf("failed to decode JSON: %w", err)
	}
	var languages []string
	for lang := range jsonData {
		languages = append(languages, lang)
	}
	sort.Strings(languages) // In Go, map iteration order is intentionally randomized
	// Each time you iterate over a map using a for range loop, the order is different.
	return languages, nil
}

func (m languageModel) View() string {

	poolIndex := getPoolIndex(m.WindowWidth, m.WindowHeight)
	b := bufferPools[poolIndex].Get().([]byte)[:0]
	defer bufferPools[poolIndex].Put(b)

	b = m.wrapAndPad(b, "> Blackjack <")
	b = append(b, newlineRune)
	b = append(b, newlineRune)
	b = m.wrapAndPad(b, "Choose Language:")
	b = append(b, newlineRune)
	b = append(b, newlineRune)
	b = m.wrapAndPad(b, "Use ↑/↓ to select and Enter to confirm:")
	b = append(b, newlineRune)
	b = append(b, newlineRune)
	b = m.wrapAndPad(b, renderOptions(paginateLines(m.languages, m.Page), m.Cursor))
	b = append(b, newlineRune)
	b = append(b, newlineRune)
	b = m.verticalPad(b)

	return string(b)
}

func (m languageModel) wrapAndPad(dst []byte, content string) []byte {
	return wrapAndPadWMToBuffer(dst, content, m.WindowWidth, 2)
}

func (m languageModel) verticalPad(dst []byte) []byte {
	return verticalPaddingToBuffer(dst, m.WindowWidth, m.WindowHeight)
}

func (m languageModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {

	var cmd tea.Cmd
	var model tea.Model

	switch message := msg.(type) {

	case tea.WindowSizeMsg:
		m.WindowWidth, m.WindowHeight, m.hideCursorPending, cmd = windowSize(m.WindowWidth, m.WindowHeight, m.hideCursorPending, message)
		return m, cmd

	case tea.KeyMsg:
		log.Printf("key %q on the language screen", message.String())
		model, cmd = keyPressLanguage(m, message)
		return model, cmd

	case customHideCursorMsg:
		m.hideCursorPending = false
		return m, tea.HideCursor

	case customErrorMsg:
		model = errorModel{message: message.err.Error()}
		return model, model.Init()

	case customFinishMsg:
		m.languages = message.languages
		// With a single language there is nothing to choose: go straight on.
		if len(m.languages) == 1 {
			return keyPressLanguage(m, tea.KeyMsg{Type: tea.KeyEnter})
		}
		return m, nil

	default:
		return m, nil
	}
}

func keyPressLanguage(m languageModel, msg tea.KeyMsg) (tea.Model, tea.Cmd) {
	switch msg.Type {

	case tea.KeyCtrlC:
		return m, tea.Quit

	case tea.KeyUp:
		if m.Cursor >= 1 {
			m.Cursor--
		}
		return m, nil

	case tea.KeyDown:
		if m.Cursor <= len(paginateLines(m.languages, m.Page))-2 {
			m.Cursor++
		}
		return m, nil

	case tea.KeyEnter:
		m.Selected = paginateLines(m.languages, m.Page)[m.Cursor]

		if m.Selected == threeHyphens && m.Cursor == 0 {
			return m, nil
		}

		if m.Selected == threeDots && m.Cursor == 0 {
			m.Page -= 1
			m.Cursor = 0
			return m, nil
		}
		numberOfOptions := len(paginateLines(m.languages, m.Page))
		if m.Selected == fourDots && m.Cursor == numberOfOptions-1 {
			m.Page += 1
			m.Cursor = 0
			return m, nil
		}

		ui, err := loadUiStrings(m.Selected)
		if err != nil {
			model := errorModel{message: err.Error()}
			return model, model.Init()
		}
		ui = finalizeUiStrings(ui)
		model := blackjackModel{
			Game:    gameState{Phase: phaseConfig, ConfigStep: configStepStartUp},
			UiState: uiState{Cursor: 0, langLoadPage: m.Page, LoadPage: 0, firstTime: true},
			UiText:  ui,
		}
		return model, model.Init()

	default:
		return m, nil
	}
}

func loadUiStrings(language string) (uiText, error) {
	file, err := openStrings(path)
	if err != nil {
		log.Println("failed to open " + path)
		return uiText{}, fmt.Errorf("%w", err)
	}
	defer func() {
		closeErr := file.Close()
		if closeErr != nil {
			log.Println("Error while closing " + path + ": " + closeErr.Error())
		}
	}()

	var langMap map[string]uiText
	err = json.NewDecoder(file).Decode(&langMap)
	if err != nil {
		log.Println("failed to decode " + path)
		return uiText{}, fmt.Errorf("%w", err)
	}
	uiStrings, ok := langMap[language]
	if !ok {
		log.Println("language '"+language+"' not found in", path)
		return uiText{}, fmt.Errorf("%v", ok)
	}
	return uiStrings, nil
}

func finalizeUiStrings(ui uiText) uiText {
	ui.SaveNotFoundNewInstead = saveFile + singleSpaceString + ui.SaveNotFoundNewInstead

	allDecks := [6]string{
		ui.OneDeck, ui.TwoDeck, ui.ThreeDeck,
		ui.FourDeck, ui.FiveDeck, ui.SixDeck,
	}
	count := 0
	for _, deck := range allDecks {
		if deck != emptyString {
			count++
		}
	}
	result := make([]string, count)
	i := 0
	for _, deck := range allDecks {
		if deck != emptyString {
			result[i] = deck
			i++
		}
	}
	ui.PhaseConfigStepDecks = result

	return ui
}

// ------------------- Error Model ----------------------------

func (m errorModel) Init() tea.Cmd {
	return tea.WindowSize()
}

func (m errorModel) View() string {

	poolIndex := getPoolIndex(m.WindowWidth, m.WindowHeight)
	b := bufferPools[poolIndex].Get().([]byte)[:0]
	defer bufferPools[poolIndex].Put(b)

	b = m.wrapAndPad(b, "X Error: "+m.message)
	b = append(b, newlineRune)
	b = append(b, newlineRune)
	b = m.wrapAndPad(b, "Press any key to exit...")
	b = append(b, newlineRune)
	b = append(b, newlineRune)
	b = m.verticalPad(b)

	return string(b)
}

func (m errorModel) wrapAndPad(dst []byte, content string) []byte {
	return wrapAndPadWMToBuffer(dst, content, m.WindowWidth, 2)
}

func (m errorModel) verticalPad(dst []byte) []byte {
	return verticalPaddingToBuffer(dst, m.WindowWidth, m.WindowHeight)
}

func (m errorModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {

	var cmd tea.Cmd

	switch message := msg.(type) {

	case tea.WindowSizeMsg:
		m.WindowWidth, m.WindowHeight, m.hideCursorPending, cmd = windowSize(m.WindowWidth, m.WindowHeight, m.hideCursorPending, message)
		return m, cmd

	case tea.KeyMsg:
		return m, tea.Quit

	case customHideCursorMsg:
		m.hideCursorPending = false
		return m, tea.HideCursor

	default:
		return m, nil
	}
}

// ------------------- Game Over Model ------------------------

func (m gameOverModel) Init() tea.Cmd {
	return tea.WindowSize()
}

func (m gameOverModel) View() string {

	poolIndex := getPoolIndex(m.WindowWidth, m.WindowHeight)
	b := bufferPools[poolIndex].Get().([]byte)[:0]
	defer bufferPools[poolIndex].Put(b)

	b = m.wrapAndPad(b, "GAME OVER - Thank you for playing")
	b = append(b, newlineRune)
	b = append(b, newlineRune)
	b = m.wrapAndPad(b, "Press Q to exit...")
	b = append(b, newlineRune)
	b = append(b, newlineRune)
	b = m.verticalPad(b)

	return string(b)

}

func (m gameOverModel) wrapAndPad(dst []byte, content string) []byte {
	return wrapAndPadWMToBuffer(dst, content, m.WindowWidth, 2)
}

func (m gameOverModel) verticalPad(dst []byte) []byte {
	return verticalPaddingToBuffer(dst, m.WindowWidth, m.WindowHeight)
}

func (m gameOverModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {

	var cmd tea.Cmd

	switch message := msg.(type) {

	case tea.WindowSizeMsg:
		m.WindowWidth, m.WindowHeight, m.hideCursorPending, cmd = windowSize(m.WindowWidth, m.WindowHeight, m.hideCursorPending, message)
		return m, cmd

	case tea.KeyMsg:
		if message.String() == "q" || message.Type == tea.KeyCtrlC {
			return m, tea.Quit
		} else {
			return m, nil
		}

	case customHideCursorMsg:
		m.hideCursorPending = false
		return m, tea.HideCursor

	default:
		return m, nil
	}
}
