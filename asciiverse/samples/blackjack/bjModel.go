package main

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"fmt"
	tea "github.com/charmbracelet/bubbletea"
	"hash/crc32"
	"io"
	"log"
	"math/rand/v2"
	"os"
	"slices"
	"strconv"
	"strings"
	"time"
)

// ------------------- Normal Model -----------

func (m blackjackModel) Init() tea.Cmd {
	return tea.WindowSize()
}

func (m blackjackModel) View() string {

	poolIndex := getPoolIndex(m.UiState.WindowWidth, m.UiState.WindowHeight)
	b := bufferPools[poolIndex].Get().([]byte)[:0]
	defer bufferPools[poolIndex].Put(b)

	if m.Game.Phase == phaseConfig {

		b = m.wrapAndPad(b, m.UiText.Title)
		b = append(b, newlineRune)
		b = append(b, newlineRune)

		switch m.Game.ConfigStep {

		case configStepStartUp:
			return renderConfigStep(b, m, m.UiText.StartUpPrompt)

		case configStepLoad:
			return renderConfigStep(b, m, m.UiText.LoadPrompt)

		case configStepLoadFail:
			return renderConfigStep(b, m, m.UiText.LoadFailStatus)

		case configStepPayout:
			return renderConfigStep(b, m, m.UiText.PayoutPrompt)

		case configStepH17:
			return renderConfigStep(b, m, m.UiText.DealerRulePrompt)

		case configStepDecks:
			return renderConfigStep(b, m, m.UiText.NumberOfDecksPrompt)

		case configStepPen:
			return renderConfigStep(b, m, m.UiText.PenetrationPrompt)

		case configStepStartConfirm:
			return renderConfigStep(b, m, m.UiText.StartConfirmPrompt)

		case configStepLoadConfirm:
			return renderConfigStep(b, m, m.UiText.LoadConfirmPrompt)
		}
	}

	titleText := m.UiText.Title + " (Cards dealt: " + strconv.Itoa(int(m.Game.CardsDealt)) + ")" + " Money: " + strconv.Itoa(int(m.Game.PlayerMoney))

	b = m.wrapAndPad(b, titleText)
	b = append(b, newlineRune)
	b = append(b, newlineRune)

	if m.Game.Phase == phaseBet {
		b = m.wrapAndPad(b, "Select Amount to Bet")
		b = append(b, newlineRune)
		b = append(b, newlineRune)
		b = m.wrapAndPad(b, m.UiText.PromptConfirm)
		b = append(b, newlineRune)
		b = append(b, newlineRune)
		b = m.wrapAndPad(b, renderOptions(currentOptions(m), m.UiState.Cursor))
		b = append(b, newlineRune)
		b = append(b, newlineRune)
		b = m.verticalPad(b)
		return string(b)
	}

	b = wrapUnconditionalTWToBuffer(b, m.UiText.PlayerCardsLabel, m.UiState.WindowWidth, 4)
	truncatedWidthP := m.UiState.WindowWidth - len(m.UiText.PlayerCardsLabel)
	var pcs string
	for _, pc := range m.Game.PlayerCards {
		rankIdx := rankToIndex(pc.Rank)
		suitIdx := suitToIndex(pc.Suit)
		pcs += bjCards.CardCodes[rankIdx][suitIdx]
	}
	b = wrapAndPadWMToBuffer(b, pcs, truncatedWidthP, 2)
	b = append(b, newlineRune)

	b = wrapUnconditionalTWToBuffer(b, m.UiText.DealerCardsLabel, m.UiState.WindowWidth, 4)
	truncatedWidthD := m.UiState.WindowWidth - len(m.UiText.DealerCardsLabel)
	if m.Game.ShowDealerHand {
		var dcs string
		for _, dc := range m.Game.DealerCards {
			rankIdx := rankToIndex(dc.Rank)
			suitIdx := suitToIndex(dc.Suit)
			dcs += bjCards.CardCodes[rankIdx][suitIdx]
		}
		b = wrapAndPadWMToBuffer(b, dcs, truncatedWidthD, 2)
		b = append(b, newlineRune)
		b = append(b, newlineRune)
	} else {
		rankIdx := rankToIndex(m.Game.DealerCards[0].Rank)
		suitIdx := suitToIndex(m.Game.DealerCards[0].Suit)
		b = wrapAndPadWMToBuffer(b, bjCards.CardCodes[rankIdx][suitIdx]+m.UiText.UnknownCard, truncatedWidthD, 2)
		b = append(b, newlineRune)
		b = append(b, newlineRune)
	}

	if m.Game.Phase == phaseEnd {
		b = m.wrapAndPad(b, m.UiState.Message)
		b = append(b, newlineRune)
		b = m.wrapAndPad(b, m.UiText.PromptConfirm)
		b = append(b, newlineRune)
		b = append(b, newlineRune)
	} else {
		b = m.wrapAndPad(b, m.UiText.PromptConfirm)
		b = append(b, newlineRune)
		b = append(b, newlineRune)
	}

	b = m.wrapAndPad(b, renderOptions(currentOptions(m), m.UiState.Cursor))
	b = append(b, newlineRune)
	b = append(b, newlineRune)
	b = m.verticalPad(b)
	return string(b)
}

func renderConfigStep(b []byte, m blackjackModel, promptText string) string {
	b = m.wrapAndPad(b, promptText)
	b = append(b, newlineRune)
	b = append(b, newlineRune)
	b = m.wrapAndPad(b, m.UiText.PromptConfirmConfig)
	b = append(b, newlineRune)
	b = append(b, newlineRune)
	b = m.wrapAndPad(b, renderOptions(currentOptions(m), m.UiState.Cursor))
	b = append(b, newlineRune)
	b = append(b, newlineRune)
	b = m.verticalPad(b)
	return string(b)
}

func (m blackjackModel) wrapAndPad(dst []byte, content string) []byte {
	return wrapAndPadWMToBuffer(dst, content, m.UiState.WindowWidth, 2)
}

func (m blackjackModel) verticalPad(dst []byte) []byte {
	return verticalPaddingToBuffer(dst, m.UiState.WindowWidth, m.UiState.WindowHeight)
}

func (m blackjackModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {

	var cmd tea.Cmd
	var model tea.Model

	switch message := msg.(type) {

	case tea.WindowSizeMsg:
		width, height, cursor := m.UiState.WindowWidth, m.UiState.WindowHeight, m.UiState.hideCursorPending
		m.UiState.WindowWidth, m.UiState.WindowHeight, m.UiState.hideCursorPending, cmd = windowSize(width, height, cursor, message)
		return m, cmd

	case tea.KeyMsg:
		log.Printf("key %q", message.String())
		model, cmd = keyPressBlackjack(m, message)
		if bm, ok := model.(blackjackModel); ok {
			g := bm.Game
			log.Printf("state phase=%d step=%d cursor=%d money=%d bet=%d player=%d dealer=%d dealt=%d",
				g.Phase, g.ConfigStep, bm.UiState.Cursor, g.PlayerMoney, g.Bet, g.PlayerTotal, g.DealerTotal, g.CardsDealt)
		}
		return model, cmd

	case customHideCursorMsg:
		m.UiState.hideCursorPending = false
		return m, tea.HideCursor

	default:
		return m, nil
	}
}

func keyPressBlackjack(m blackjackModel, msg tea.KeyMsg) (tea.Model, tea.Cmd) {

	switch msg.Type {
	case tea.KeyCtrlC:
		return m, tea.Quit

	case tea.KeyUp:
		if m.UiState.Cursor >= 1 {
			m.UiState.Cursor--
		}
		return m, nil

	case tea.KeyDown:
		if m.UiState.Cursor <= len(currentOptions(m))-2 {
			m.UiState.Cursor++
		}
		return m, nil

	case tea.KeyEnter:
		selected := currentOptions(m)[m.UiState.Cursor]
		if m.Game.Phase == phaseConfig {
			return handleConfigSelection(m, selected)
		} else if m.Game.Phase == phaseBet {
			return handleBetSelection(m, selected)
		} else {
			return handleSelection(m, selected)
		}

	case tea.KeyBackspace:
		if m.Game.Phase == phaseConfig {
			return handleConfigBackstep(m)
		} else {
			return m, nil
		}

	default:
		return m, nil
	}
}

func currentOptions(m blackjackModel) []string {

	if m.Game.Phase == phasePlay || m.Game.Phase == phaseEnd {
		switch {
		case m.Game.Phase == phasePlay && m.Game.PlayerMoney < m.Game.Bet:
			return []string{m.UiText.OptionHit, m.UiText.OptionStand, m.UiText.OptionSurrender}
		case m.Game.Phase == phasePlay && m.Game.PlayerMoney >= m.Game.Bet:
			return []string{m.UiText.OptionHit, m.UiText.OptionStand, m.UiText.OptionDouble, m.UiText.OptionSurrender}
		case m.Game.Phase == phaseEnd:
			return []string{m.UiText.OptionRestart, m.UiText.OptionQuit, m.UiText.SaveAndQuit}
		default:
			return nil
		}
	}

	if m.Game.Phase == phaseBet {
		switch {
		case m.Game.PlayerMoney >= 50:
			return []string{bet10, bet20, bet30, bet40, bet50}
		case m.Game.PlayerMoney >= 40:
			return []string{bet10, bet20, bet30, bet40}
		case m.Game.PlayerMoney >= 30:
			return []string{bet10, bet20, bet30}
		case m.Game.PlayerMoney >= 20:
			return []string{bet10, bet20}
		case m.Game.PlayerMoney >= 10:
			return []string{bet10}
		}
	}

	switch m.Game.ConfigStep {
	case configStepStartUp:
		return []string{m.UiText.StartNewGame, m.UiText.LoadOldGame}

	case configStepLoad:
		if len(m.UiState.Saves) > 0 {
			return paginateLines(m.UiState.Saves, m.UiState.LoadPage)
		}
		if !m.UiState.firstTime {
			log.Print("repeat fail to load save file")
			return []string{m.UiText.SaveNotFoundNewInstead, m.UiText.OptionQuit}
		}

		saves, err := loadSaveFile()
		if err != nil || len(saves) == 0 {
			return []string{m.UiText.SaveNotFoundNewInstead, m.UiText.OptionQuit}
		}
		return paginateLines(saves, m.UiState.LoadPage)

	case configStepLoadFail:
		return []string{m.UiText.StartNewGame, m.UiText.OptionQuit}

	case configStepPayout:
		return []string{payout32, payout75, payout65}

	case configStepH17:
		return []string{m.UiText.DealerHit17, m.UiText.DealerStand17}

	case configStepDecks:
		return m.UiText.PhaseConfigStepDecks

	case configStepPen:
		if m.Game.NumberDecks >= 2 {
			return []string{pen00, pen25, pen50, pen75}
		} else {
			return []string{pen00, pen25, pen50}
		}

	case configStepStartConfirm:
		return []string{m.UiText.StartNewGame}

	case configStepLoadConfirm:
		return []string{m.UiText.LoadOldGame}

	default:
		return nil
	}
}

func handleConfigSelection(m blackjackModel, selected string) (tea.Model, tea.Cmd) {

	switch m.Game.ConfigStep {

	case configStepStartUp:
		if selected == m.UiText.StartNewGame {
			m.Game.ConfigStep = configStepPayout
		} else {
			m.Game.ConfigStep = configStepLoad
		}
		m.UiState.Cursor = 0
		return m, nil

	case configStepLoad:
		return handleConfigStepLoad(m, selected)

	case configStepLoadConfirm, configStepStartConfirm:
		m.Game.Phase = phaseBet
		return m, nil

	case configStepLoadFail:
		if selected == m.UiText.OptionQuit {
			return m, tea.Quit
		} else if selected == m.UiText.StartNewGame {
			m.Game.ConfigStep = configStepPayout
		}
		m.UiState.Cursor = 0
		return m, nil

	case configStepPayout:
		switch selected {
		case payout32:
			m.Game.Payout = 15
		case payout75:
			m.Game.Payout = 14
		case payout65:
			m.Game.Payout = 12
		}
		m.Game.ConfigStep = configStepH17
		m.UiState.Cursor = 0
		return m, nil

	case configStepH17:
		m.Game.HitOnSoft17 = selected == m.UiText.DealerHit17
		m.Game.ConfigStep = configStepDecks
		m.UiState.Cursor = 0
		return m, nil

	case configStepDecks:
		match := regexIntegers.FindString(selected)
		deckCount, err := strconv.Atoi(match)
		if err != nil || deckCount < 1 || deckCount > 255 {
			log.Println("Failed to extract number of decks from " + selected)
			deckCount = 1
		}
		m.Game.NumberDecks = uint8(deckCount)
		m.Game.ConfigStep = configStepPen
		m.UiState.Cursor = 0
		return m, nil

	case configStepPen:
		return handleConfigStepPen(m, selected)

	default:
		return m, nil
	}
}

func handleConfigStepLoad(m blackjackModel, selected string) (tea.Model, tea.Cmd) {
	if selected == m.UiText.SaveNotFoundNewInstead {
		m.Game.ConfigStep = configStepPayout
		m.UiState.Cursor = 0
		return m, nil
	}
	if selected == m.UiText.OptionQuit {
		return m, tea.Quit
	}
	if selected == threeHyphens && m.UiState.Cursor == 0 {
		return m, nil
	}
	if selected == threeDots && m.UiState.Cursor == 0 {
		m.UiState.LoadPage -= 1
		m.Game.ConfigStep = configStepLoad
		m.UiState.Cursor = 0
		return m, nil
	}
	numberOfOptions := len(currentOptions(m))
	if selected == fourDots && m.UiState.Cursor == numberOfOptions-1 {
		m.UiState.LoadPage += 1
		m.Game.ConfigStep = configStepLoad
		m.UiState.Cursor = 0
		return m, nil
	}
	loadedGameState, err := decodeGameState(selected)
	log.Print(err)
	if err != nil {
		m.Game.ConfigStep = configStepLoadFail
		m.UiState.Cursor = 0
		return m, nil
	}

	m.Game.RandomSeed = loadedGameState.RandomSeed
	m.Game.ReshuffleThreshold = loadedGameState.ReshuffleThreshold
	m.Game.CardsDealt = loadedGameState.CardsDealt
	m.Game.NumberDecks = loadedGameState.NumberDecks
	m.Game.HitOnSoft17 = loadedGameState.HitOnSoft17
	m.Game.NeedReshuffle = loadedGameState.NeedReshuffle
	m.Game.PlayerMoney = loadedGameState.PlayerMoney
	m.Game.Payout = loadedGameState.Payout
	totalCards := uint16(m.Game.NumberDecks) * 52
	drawStack := make([]card, 0, totalCards)
	m.Game.DrawStack, m.Game.RandomSeed = newDeck(m.Game.NumberDecks, drawStack, m.Game.RandomSeed)
	m.Game.DrawStack = m.Game.DrawStack[m.Game.CardsDealt:]
	playerCards := make([]card, 0, 22) // 22xA
	dealerCards := make([]card, 0, 13) // 7xA + 1x5 + 5xA
	m.Game.PlayerCards = playerCards
	m.Game.DealerCards = dealerCards

	m.Game.ConfigStep = configStepLoadConfirm
	m.UiState.Cursor = 0
	return m, nil
}

func handleConfigStepPen(m blackjackModel, selected string) (tea.Model, tea.Cmd) {
	var penetration uint8
	switch selected {
	case pen00:
		penetration = 0
	case pen25:
		penetration = 25
	case pen50:
		penetration = 50
	case pen75:
		penetration = 75
	default:
		log.Println("Failed to extract penetration percent from " + selected)
		penetration = 50
	}
	totalCards := uint16(m.Game.NumberDecks) * 52
	m.Game.ReshuffleThreshold = (totalCards * uint16(penetration)) / 100
	drawStack := make([]card, 0, totalCards)
	m.Game.DrawStack, m.Game.RandomSeed = newDeck(m.Game.NumberDecks, drawStack)
	playerCards := make([]card, 0, 22) // 22xA
	dealerCards := make([]card, 0, 13) // 7xA + 1x5 + 5xA
	m.Game.PlayerMoney = 100
	m.Game.PlayerCards = playerCards
	m.Game.DealerCards = dealerCards
	m.Game.CardsDealt = 0
	m.Game.NeedReshuffle = false
	m.Game.ConfigStep = configStepStartConfirm
	m.UiState.Cursor = 0
	return m, nil
}

func handleBetSelection(m blackjackModel, selected string) (tea.Model, tea.Cmd) {
	switch selected {
	case bet10:
		m.Game.Bet = 10
	case bet20:
		m.Game.Bet = 20
	case bet30:
		m.Game.Bet = 30
	case bet40:
		m.Game.Bet = 40
	case bet50:
		m.Game.Bet = 50
	}
	m.Game.PlayerMoney -= m.Game.Bet
	startNewGameModel := gameModel(m)
	return startNewGameModel, startNewGameModel.Init()
}

func handleSelection(m blackjackModel, selected string) (tea.Model, tea.Cmd) {

	switch selected {
	case m.UiText.OptionHit, m.UiText.OptionStand, m.UiText.OptionDouble:
		if m.Game.Turn != turnPlayer {
			return m, nil
		}
	}

	switch selected {
	case m.UiText.OptionHit:
		return hit(m, normalLoose), nil

	case m.UiText.OptionStand:
		return stand(m, normalWin, normalLoose, normalDraw), nil

	case m.UiText.OptionDouble:
		m = hit(m, doubleLoose)
		if m.Game.Phase == phaseEnd {
			return m, nil
		}
		return stand(m, doubleWin, doubleLoose, doubleDraw), nil

	case m.UiText.OptionSurrender:
		m.Game.ShowDealerHand = true
		m.Game.Phase = phaseEnd
		m.UiState.Cursor = 0
		m.UiState.Message = m.UiText.Surrendered
		m.Game.PlayerMoney, m.UiState.Message = calculateMoney(m.Game.Payout, m.Game.Bet, m.Game.PlayerMoney, m.UiState.Message, surrender)
		return m, nil

	case m.UiText.OptionRestart:
		if int(m.Game.PlayerMoney) < 10 {
			model := gameOverModel{
				WindowHeight: m.UiState.WindowHeight,
				WindowWidth:  m.UiState.WindowWidth,
			}
			return model, model.Init()
		}
		if m.Game.NeedReshuffle {
			m.Game.DrawStack, m.Game.RandomSeed = newDeck(m.Game.NumberDecks, m.Game.DrawStack)
			m.Game.NeedReshuffle = false
			m.Game.CardsDealt = 0
		}

		m.Game.Phase = phaseBet
		return m, nil

	case m.UiText.OptionQuit:
		return m, tea.Quit

	case m.UiText.SaveAndQuit:
		return saveGameAndQuit(m), tea.Quit

	default:
		return m, nil
	}
}

func hit(m blackjackModel, payoutType int) blackjackModel {
	m.Game.PlayerCards, m.Game.DrawStack = drawOneFromStack(m.Game.PlayerCards, m.Game.DrawStack)
	m.Game.CardsDealt++
	m.Game.PlayerTotal, _ = calculateHand(m.Game.PlayerCards)

	if m.Game.PlayerTotal >= 22 {
		m.UiState.Message = m.UiText.PlayerBusts + strconv.Itoa(m.Game.PlayerTotal) + ". " + m.UiText.DealerWins
		m.Game.ShowDealerHand = true
		m.Game.Phase = phaseEnd
		m.UiState.Cursor = 0
		m.Game.PlayerMoney, m.UiState.Message = calculateMoney(m.Game.Payout, m.Game.Bet, m.Game.PlayerMoney, m.UiState.Message, payoutType)
	}

	if m.Game.CardsDealt >= m.Game.ReshuffleThreshold {
		m.Game.NeedReshuffle = true
	}
	return m
}

func stand(m blackjackModel, winPayout, loosePayout, drawPayout int) blackjackModel {
	m.Game.ShowDealerHand = true
	m.Game.Turn = turnDealer

	for m.Game.DealerTotal <= 16 || (m.Game.DealerTotal == 17 && m.Game.HitOnSoft17 && m.Game.IsSoft17) {
		m.Game.DealerCards, m.Game.DrawStack = drawOneFromStack(m.Game.DealerCards, m.Game.DrawStack)
		m.Game.CardsDealt++
		m.Game.DealerTotal, m.Game.IsSoft17 = calculateHand(m.Game.DealerCards)
	}

	switch {
	case m.Game.DealerTotal >= 22:
		m.UiState.Message = m.UiText.DealerBusts + strconv.Itoa(m.Game.DealerTotal) + ". " + m.UiText.PlayerWins
		m.Game.PlayerMoney, m.UiState.Message = calculateMoney(m.Game.Payout, m.Game.Bet, m.Game.PlayerMoney, m.UiState.Message, winPayout)
	case m.Game.PlayerTotal > m.Game.DealerTotal:
		m.UiState.Message = m.UiText.PlayerWins + strconv.Itoa(m.Game.PlayerTotal) + " > " + strconv.Itoa(m.Game.DealerTotal)
		m.Game.PlayerMoney, m.UiState.Message = calculateMoney(m.Game.Payout, m.Game.Bet, m.Game.PlayerMoney, m.UiState.Message, winPayout)
	case m.Game.PlayerTotal < m.Game.DealerTotal:
		m.UiState.Message = m.UiText.DealerWins + strconv.Itoa(m.Game.PlayerTotal) + " < " + strconv.Itoa(m.Game.DealerTotal)
		m.Game.PlayerMoney, m.UiState.Message = calculateMoney(m.Game.Payout, m.Game.Bet, m.Game.PlayerMoney, m.UiState.Message, loosePayout)
	case m.Game.PlayerTotal == m.Game.DealerTotal:
		m.UiState.Message = m.UiText.Draw + strconv.Itoa(m.Game.PlayerTotal) + " = " + strconv.Itoa(m.Game.DealerTotal)
		m.Game.PlayerMoney, m.UiState.Message = calculateMoney(m.Game.Payout, m.Game.Bet, m.Game.PlayerMoney, m.UiState.Message, drawPayout)
	}

	m.Game.Phase = phaseEnd
	m.UiState.Cursor = 0
	if m.Game.CardsDealt >= m.Game.ReshuffleThreshold {
		m.Game.NeedReshuffle = true
	}
	return m
}

func saveGameAndQuit(m blackjackModel) blackjackModel {
	saveFilePath := statePath(saveFile)
	file, err := os.OpenFile(saveFilePath, os.O_RDWR|os.O_CREATE, 0o644)
	if err != nil {
		log.Printf("Error opening/creating file: %v\n", err)
		return m
	}
	defer func() {
		closeErr := file.Close()
		if closeErr != nil {
			log.Println("Error while closing " + saveFilePath + ": " + closeErr.Error())
		}
	}()
	existingContent, err := io.ReadAll(file)
	if err != nil {
		log.Printf("Error reading file: %v\n", err)
		return m
	}
	existingContent, err = os.ReadFile(saveFilePath)
	if err != nil {
		log.Printf("Error reading existing file: %v\n", err)
		return m
	}
	stringEncoded, err := m.Game.encodeWithChecksum()
	if err != nil {
		log.Fatalf("Base32 encoding failed: %v", err)
	}
	timestamp := time.Now().Format("2006-01-02-15:04:05")
	lineToAdd := timestamp + singleSpaceString + stringEncoded + "\n"
	newContent := make([]byte, 0, len([]byte(lineToAdd))+len(existingContent))
	newContent = append(newContent, []byte(lineToAdd)...)
	newContent = append(newContent, existingContent...)
	err = os.WriteFile(saveFilePath, newContent, 0o644)
	if err != nil {
		log.Printf("Error writing to file: %v\n", err)
		return m
	}
	return m
}

func handleConfigBackstep(m blackjackModel) (tea.Model, tea.Cmd) {

	switch m.Game.ConfigStep {
	case configStepLoad, configStepLoadFail, configStepPayout, configStepH17, configStepDecks, configStepPen, configStepStartConfirm, configStepLoadConfirm:
		m.UiState.Cursor = 0
	}

	switch m.Game.ConfigStep {

	case configStepStartUp:
		model := languageModel{Cursor: 0, Page: m.UiState.langLoadPage}
		return model, model.Init()

	case configStepLoad:
		m.Game.ConfigStep = configStepStartUp
		return m, nil

	case configStepLoadFail, configStepLoadConfirm:
		m.Game.ConfigStep = configStepLoad
		return m, nil

	case configStepPayout:
		m.Game.ConfigStep = configStepStartUp
		return m, nil

	case configStepH17:
		m.Game.ConfigStep = configStepPayout
		return m, nil

	case configStepDecks:
		m.Game.ConfigStep = configStepH17
		return m, nil

	case configStepPen:
		m.Game.ConfigStep = configStepDecks
		return m, nil

	case configStepStartConfirm:
		m.Game.ConfigStep = configStepPen
		return m, nil

	default:
		return m, nil
	}
}

func gameModel(m blackjackModel) blackjackModel {

	m.UiState.Message = emptyString
	m.Game.PlayerCards = m.Game.PlayerCards[:0]
	m.Game.DealerCards = m.Game.DealerCards[:0]
	m.Game.Turn = turnPlayer
	m.UiState.Cursor = 0
	m.Game.Phase = phasePlay
	m.Game.ShowDealerHand = false

	m.Game.PlayerCards, m.Game.DrawStack = drawOneFromStack(m.Game.PlayerCards, m.Game.DrawStack)
	m.Game.DealerCards, m.Game.DrawStack = drawOneFromStack(m.Game.DealerCards, m.Game.DrawStack)
	m.Game.PlayerCards, m.Game.DrawStack = drawOneFromStack(m.Game.PlayerCards, m.Game.DrawStack)
	m.Game.DealerCards, m.Game.DrawStack = drawOneFromStack(m.Game.DealerCards, m.Game.DrawStack)
	m.Game.CardsDealt += 4
	if m.Game.CardsDealt >= m.Game.ReshuffleThreshold {
		m.Game.NeedReshuffle = true
	}
	m.Game.PlayerTotal, _ = calculateHand(m.Game.PlayerCards)
	m.Game.DealerTotal, m.Game.IsSoft17 = calculateHand(m.Game.DealerCards)

	if m.Game.PlayerTotal == 21 || m.Game.DealerTotal == 21 {
		m.Game.ShowDealerHand = true
		m.Game.Phase = phaseEnd
		//goland:noinspection ALL
		switch {
		case m.Game.PlayerTotal == 21 && m.Game.DealerTotal != 21:
			m.UiState.Message = m.UiText.NaturalBlackjackPlayer
			m.Game.PlayerMoney, m.UiState.Message = calculateMoney(m.Game.Payout, m.Game.Bet, m.Game.PlayerMoney, m.UiState.Message, naturalBlackjackWin)
		case m.Game.PlayerTotal != 21 && m.Game.DealerTotal == 21:
			m.UiState.Message = m.UiText.NaturalBlackjackDealer
			m.Game.PlayerMoney, m.UiState.Message = calculateMoney(m.Game.Payout, m.Game.Bet, m.Game.PlayerMoney, m.UiState.Message, normalLoose)
		case m.Game.PlayerTotal == 21 && m.Game.DealerTotal == 21:
			m.UiState.Message = m.UiText.NaturalBlackjackDraw
			m.Game.PlayerMoney, m.UiState.Message = calculateMoney(m.Game.Payout, m.Game.Bet, m.Game.PlayerMoney, m.UiState.Message, normalDraw)
		}
	}
	return m
}

func calculateMoney(payout uint8, bet, playerMoney uint16, message string, outcome int) (uint16, string) {
	if playerMoney >= 65000 && (outcome == naturalBlackjackWin || outcome == normalWin) {
		message += " But Dealer is broke - no more winnable money - GG"
		return playerMoney, message
	}

	switch outcome {

	case naturalBlackjackWin:
		part := uint16(10 + payout)
		playerMoney += bet * part / 10 // ⚠️ integer division
	case normalWin:
		playerMoney += bet * 2
	case doubleWin:
		playerMoney += bet * 3
	case normalDraw:
		playerMoney += bet
	case doubleDraw:
		playerMoney += bet
	case normalLoose:
		// no change to playerMoney
		// due to starting bet. You already lost the starting bet, as such no change.
		// If you draw you get your bet back etc.
	case doubleLoose:
		playerMoney -= bet
	case surrender:
		playerMoney += bet / 2
	}

	return playerMoney, message
}

// ------------------- Supporting Functions -------------------

func loadSaveFile() ([]string, error) {
	file, err := os.Open(statePath(saveFile))
	if err != nil {
		return nil, fmt.Errorf("error opening %s: %w", saveFile, err)
	}
	defer func() {
		closeErr := file.Close()
		if closeErr != nil {
			log.Printf("error closing %s: %v", saveFile, closeErr)
		}
	}()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line != emptyString {
			lines = append(lines, line)
		}
	}

	err = scanner.Err()
	if err != nil {
		return nil, fmt.Errorf("error scanning %s: %w", saveFile, err)
	}

	return lines, nil
}

func newDeck(deckCount uint8, drawStack []card, seed ...uint32) ([]card, uint32) {

	var usedSeed uint32
	if len(seed) > 0 {
		usedSeed = seed[0]
	} else {
		// casting a larger integer (int64) to a smaller one (uint32) truncates the higher bits
		// keeping the lower bits: [High bits] [Low bits] <- the lower 32 bits change frequently
		usedSeed = uint32(time.Now().UnixMilli()) // Wraparound every 49.7 days
	}
	drawStack = drawStack[:0]
	var i uint8
	for i = 0; i < deckCount; i++ {
		drawStack = append(drawStack, bjCards.StandardDeck...)
	}

	rng := rand.New(rand.NewPCG(expandUint32ToTwoUint64(usedSeed)))
	rng.Shuffle(len(drawStack), func(i, j int) {
		drawStack[i], drawStack[j] = drawStack[j], drawStack[i]
	})
	return drawStack, usedSeed
}

func expandUint32ToTwoUint64(x uint32) (uint64, uint64) {
	y := uint64(x)
	s1 := y | (y << 32)
	top16 := (y & 0xFFFF0000) << 48
	middle32 := y << 16
	bottom16 := y & 0x0000FFFF
	s2 := top16 | middle32 | bottom16
	// s1 = full seed | full seed
	// s2 = top16 | full seed | bottom16
	return s1, s2
}

func drawOneFromStack(hand []card, drawStack []card) ([]card, []card) {

	drawnCard := drawStack[0]
	hand = append(hand, drawnCard)
	return hand, drawStack[1:]
}

func calculateHand(hand []card) (total int, isSoft17 bool) {

	total = 0
	aces := 0
	for _, c := range hand {
		total += c.Value
		if c.Rank == ace {
			aces++
		}
	}
	for aces >= 1 && total >= 22 {
		total -= 10
		aces--
	}
	if total == 17 && aces >= 1 {
		return total, true
	} else {
		return total, false
	}
}

// ------------------- Save / Load Functionality --------------

func (gs gameState) encodeWithChecksum() (string, error) {
	savableState := savableGameState{
		RandomSeed:         gs.RandomSeed,
		playerMoney:        gs.PlayerMoney,
		ReshuffleThreshold: gs.ReshuffleThreshold,
		CardsDealt:         gs.CardsDealt,
		NumberDecks:        gs.NumberDecks,
		HitOnSoft17:        gs.HitOnSoft17,
		NeedReshuffle:      gs.NeedReshuffle,
		Payout:             gs.Payout,
	}
	buf := make([]byte, 13)
	binary.LittleEndian.PutUint32(buf[0:4], savableState.RandomSeed)
	binary.LittleEndian.PutUint16(buf[4:6], savableState.ReshuffleThreshold)
	binary.LittleEndian.PutUint16(buf[6:8], savableState.CardsDealt)
	buf[8] = savableState.NumberDecks
	var flags byte
	if savableState.HitOnSoft17 {
		flags |= 1
	}
	if savableState.NeedReshuffle {
		flags |= 2
	}
	buf[9] = flags
	binary.LittleEndian.PutUint16(buf[10:12], savableState.playerMoney)
	buf[12] = savableState.Payout
	encoded := encodeBase45(buf)
	// 20-bit truncated CRC32 ~99.9999% accuracy (1 in ~1M collision rate)
	checksum := crc32.ChecksumIEEE(buf) & 0xFFFFF // 0xFFFFF = 1048575 = 20 bits
	checksumStr := encodeBase32From20Bits(checksum)
	return encoded + equalSignString + checksumStr, nil
}

func encodeBase45(data []byte) string {
	if len(data) == 0 {
		return emptyString
	}

	leadingZeros := 0
	for _, b := range data {
		if b == 0 {
			leadingZeros++
		} else {
			break
		}
	}

	if leadingZeros == len(data) {
		return string(bytes.Repeat([]byte{zeroRune}, len(data)))
	}

	temp := make([]byte, 0, len(data)-leadingZeros)
	temp = append(temp, data[leadingZeros:]...)
	result := make([]byte, 0, len(temp)*146/100+1) // log(2)/log(45) ≈ 0.1821 base45 digits per bit ~ 1.46 characters per byte

	for start := leadingZeros; start < len(temp); {
		remainder := divideBy45From(temp, start)
		for start < len(temp) && temp[start] == 0 {
			start++
		}
		result = append(result, base45Chars[remainder])
	}

	for i, j := 0, len(result)-1; i < j; i, j = i+1, j-1 {
		result[i], result[j] = result[j], result[i]
	}

	if leadingZeros > 0 {
		final := make([]byte, leadingZeros+len(result))
		for i := 0; i < leadingZeros; i++ {
			final[i] = zeroRune
		}
		copy(final[leadingZeros:], result)
		return string(final)
	}

	return string(result)
}

func divideBy45From(data []byte, start int) int {
	remainder := 0
	for i := start; i < len(data); i++ {
		temp := remainder*256 + int(data[i])
		data[i] = byte(temp / 45)
		remainder = temp % 45
	}
	return remainder
}

func decodeBase45(encoded string) ([]byte, error) {
	if len(encoded) == 0 {
		return []byte{}, nil
	}

	leadingZeros := 0
	for leadingZeros < len(encoded) && encoded[leadingZeros] == zeroRune {
		leadingZeros++
	}

	if leadingZeros == len(encoded) {
		return make([]byte, leadingZeros), nil
	}

	for i := 0; i < len(encoded); i++ {
		r := encoded[i]
		if base45Lookup[r] == -1 {
			return []byte{}, fmt.Errorf("invalid character at position %d: not in Base45 alphabet", i)
		}
	}

	nonZeroLen := len(encoded) - leadingZeros
	estimatedSize := int(float64(nonZeroLen)*0.687 + 1) // Base45 -> Base256: log(45)/log(256) ≈ 0.687
	if estimatedSize < 8 {
		estimatedSize = 8
	}

	result := make([]byte, 1, estimatedSize)
	result[0] = 0

	for i := leadingZeros; i < len(encoded); i++ {
		r := encoded[i]
		val := base45Lookup[r]
		// Inlined multiply by 45 and add - performance critical path
		carry := int(val)
		for j := len(result) - 1; j >= 0; j-- {
			carry += int(result[j]) * 45
			result[j] = byte(carry)
			carry >>= 8
		}
		if carry > 0 {
			result = handleCarryOverflow(result, carry)
		}
	}

	if leadingZeros == 0 {
		return result, nil
	}

	decoded := make([]byte, leadingZeros, leadingZeros+len(result))
	decoded = append(decoded, result...)
	return decoded, nil
}

// extracted for readability
func handleCarryOverflow(result []byte, carry int) []byte {
	carryBytes := 0
	tempCarry := carry
	for tempCarry > 0 {
		carryBytes++
		tempCarry >>= 8
	}

	newLen := len(result) + carryBytes
	oldLen := len(result)
	if cap(result) < newLen {
		growthNeeded := newLen - cap(result)
		result = slices.Grow(result, growthNeeded)
	}
	result = result[:newLen]

	result = append(result[:carryBytes], result[:oldLen]...)

	for j := carryBytes - 1; j >= 0; j-- {
		result[j] = byte(carry)
		carry >>= 8
	}

	return result
}

func encodeBase32From20Bits(value uint32) string {
	char1 := base32Chars[(value>>15)&0x1F] // Bits 19-15
	char2 := base32Chars[(value>>10)&0x1F] // Bits 14-10
	char3 := base32Chars[(value>>5)&0x1F]  // Bits 9-5
	char4 := base32Chars[value&0x1F]       // Bits 4-0
	return string([]byte{char1, char2, char3, char4})
}

func decodeBase32To20Bits(s string) (uint32, error) {
	if len(s) != 4 {
		return 0, fmt.Errorf("checksum must be exactly 4 characters")
	}
	s = strings.ToUpper(s)
	var value uint32
	for i, char := range s {
		idx := strings.IndexByte(base32Chars, byte(char))
		if idx == -1 {
			return 0, fmt.Errorf("invalid base32 character: %c", char)
		}
		switch i {
		case 0: // Bits 19-15
			value |= uint32(idx) << 15
		case 1: // Bits 14-10
			value |= uint32(idx) << 10
		case 2: // Bits 9-5
			value |= uint32(idx) << 5
		case 3: // Bits 4-0
			value |= uint32(idx)
		}
	}
	return value, nil
}

// Format: "timestamp base45string=base32checksum"
func parseSaveString(input string) (timestamp, base45string, base32checksum string, err error) {

	if strings.Count(input, singleSpaceString) != 1 {
		return emptyString, emptyString, emptyString, fmt.Errorf("invalid format: expected exactly one space separator")
	}
	if strings.Count(input, equalSignString) != 1 {
		return emptyString, emptyString, emptyString, fmt.Errorf("invalid format: expected exactly one '=' separator")
	}

	timestamp, rest, _ := strings.Cut(input, singleSpaceString)
	base45string, base32checksum, _ = strings.Cut(rest, equalSignString)

	return timestamp, base45string, base32checksum, nil
}

func decodeGameState(saveString string) (gameState, error) {
	_, base45String, base32Checksum, err := parseSaveString(saveString)
	if err != nil {
		return gameState{}, fmt.Errorf("failed to parse save string: %v", err)
	}
	decodedBase45, err := decodeBase45(base45String)
	if err != nil {
		return gameState{}, fmt.Errorf("failed to decode base45: %v", err)
	}
	if len(decodedBase45) != 13 {
		return gameState{}, fmt.Errorf("invalid data length: expected 13 bytes, got %d", len(decodedBase45))
	}
	decodedBase32, err := decodeBase32To20Bits(base32Checksum)
	if err != nil {
		return gameState{}, fmt.Errorf("failed to decode checksum: %v", err)
	}
	expectedChecksum := crc32.ChecksumIEEE(decodedBase45) & 0xFFFFF
	if decodedBase32 != expectedChecksum {
		return gameState{}, fmt.Errorf("checksum mismatch: expected %d, got %d", expectedChecksum, decodedBase32)
	}

	buf := decodedBase45
	savableState := savableGameState{
		RandomSeed:         binary.LittleEndian.Uint32(buf[0:4]),
		ReshuffleThreshold: binary.LittleEndian.Uint16(buf[4:6]),
		CardsDealt:         binary.LittleEndian.Uint16(buf[6:8]),
		NumberDecks:        buf[8],
		HitOnSoft17:        (buf[9] & 1) != 0,
		NeedReshuffle:      (buf[9] & 2) != 0,
		playerMoney:        binary.LittleEndian.Uint16(buf[10:12]),
		Payout:             buf[12],
	}

	gs := gameState{
		RandomSeed:         savableState.RandomSeed,
		PlayerMoney:        savableState.playerMoney,
		ReshuffleThreshold: savableState.ReshuffleThreshold,
		CardsDealt:         savableState.CardsDealt,
		NumberDecks:        savableState.NumberDecks,
		HitOnSoft17:        savableState.HitOnSoft17,
		NeedReshuffle:      savableState.NeedReshuffle,
		Payout:             savableState.Payout,
	}

	return gs, nil
}
