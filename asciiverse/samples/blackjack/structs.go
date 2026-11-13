package main

// https://github.com/dkorunic/betteralign

// ------------------- miscModels -----------------------------

type languageModel struct {
	Selected          string
	languages         []string
	Cursor            int
	WindowWidth       int
	WindowHeight      int
	Page              int
	hideCursorPending bool
}

type errorModel struct {
	message           string
	WindowWidth       int
	WindowHeight      int
	hideCursorPending bool
}

type gameOverModel struct {
	message           string
	WindowWidth       int
	WindowHeight      int
	hideCursorPending bool
}

type customHideCursorMsg struct{}
type customErrorMsg struct{ err error }
type customFinishMsg struct{ languages []string }

// ------------------- bjModel --------------------------------

type uiText struct {
	Title                  string   `json:"title"`
	DealerRulePrompt       string   `json:"dealer_rule_prompt"`
	NumberOfDecksPrompt    string   `json:"number_of_decks_prompt"`
	PlayerCardsLabel       string   `json:"player_cards_label"`
	DealerCardsLabel       string   `json:"dealer_cards_label"`
	UnknownCard            string   `json:"unknown_card"`
	PromptConfirm          string   `json:"prompt_confirm"`
	PromptConfirmConfig    string   `json:"prompt_confirm_config"`
	NaturalBlackjackPlayer string   `json:"natural_blackjack_player"`
	NaturalBlackjackDealer string   `json:"natural_blackjack_dealer"`
	NaturalBlackjackDraw   string   `json:"natural_blackjack_draw"`
	PayoutPrompt           string   `json:"payout_prompt"`
	PlayerBusts            string   `json:"player_busts"`
	DealerBusts            string   `json:"dealer_busts"`
	PlayerWins             string   `json:"player_wins"`
	DealerWins             string   `json:"dealer_wins"`
	Draw                   string   `json:"draw"`
	OptionHit              string   `json:"option_hit"`
	OptionStand            string   `json:"option_stand"`
	OptionQuit             string   `json:"option_quit"`
	OptionRestart          string   `json:"option_restart"`
	OptionDouble           string   `json:"option_double"`
	OptionSurrender        string   `json:"option_surrender"`
	DealerHit17            string   `json:"dealer_hit17"`
	DealerStand17          string   `json:"dealer_stand17"`
	OneDeck                string   `json:"1deck"`
	TwoDeck                string   `json:"2deck"`
	ThreeDeck              string   `json:"3deck"`
	FourDeck               string   `json:"4deck"`
	FiveDeck               string   `json:"5deck"`
	SixDeck                string   `json:"6deck"`
	PenetrationPrompt      string   `json:"penetration_prompt"`
	StartConfirmPrompt     string   `json:"start_confirm_prompt"`
	LoadConfirmPrompt      string   `json:"load_confirm_prompt"`
	SaveAndQuit            string   `json:"save-and-quit"`
	StartNewGame           string   `json:"start-new-game"`
	LoadOldGame            string   `json:"load-old-game"`
	SaveNotFoundNewInstead string   `json:"save-not-found-new-instead"`
	StartUpPrompt          string   `json:"start-up-prompt"`
	LoadPrompt             string   `json:"load-prompt"`
	LoadFailStatus         string   `json:"load-fail-status"`
	Surrendered            string   `json:"option_surrender_msg"`
	PhaseConfigStepDecks   []string `json:"-"`
}

type card struct {
	Rank  string
	Suit  string
	Value int
}

type blackjackCards struct {
	CardCodes    [13][4]string
	StandardDeck []card
}

type gameState struct {
	// smallest possible data types for smallest possible base45 string encoding
	// non saved fields are just int to avoid casting, memory is actually not too important
	DrawStack          []card
	PlayerCards        []card
	DealerCards        []card
	PlayerTotal        int
	DealerTotal        int
	Turn               int
	Phase              int
	ConfigStep         int
	RandomSeed         uint32
	Bet                uint16
	PlayerMoney        uint16
	ReshuffleThreshold uint16
	CardsDealt         uint16
	NumberDecks        uint8
	Payout             uint8
	ShowDealerHand     bool
	HitOnSoft17        bool
	IsSoft17           bool
	NeedReshuffle      bool
}

type savableGameState struct {
	RandomSeed         uint32 // drawStack can be recalculated through RandomSeed
	playerMoney        uint16
	ReshuffleThreshold uint16
	CardsDealt         uint16
	NumberDecks        uint8
	Payout             uint8
	HitOnSoft17        bool
	NeedReshuffle      bool
}

type uiState struct {
	Message           string
	Saves             []string
	Cursor            int
	langLoadPage      int
	LoadPage          int
	WindowWidth       int
	WindowHeight      int
	firstTime         bool
	hideCursorPending bool
}

type blackjackModel struct {
	// padding, such that each field starts at a cache line boundary, won't help; because
	// Concurrent operations communicate through messages, not direct struct access
	// The main Update loop is always single-threaded
	// Models are copied, not shared between goroutines and
	// Background goroutines don't modify the model directly
	UiText  uiText
	UiState uiState
	Game    gameState
}
