package main

import (
	"regexp"
	"sync"
)

var (
	cachedSpacesString string
	cachedSpacesWidth  int
)

// ------------------- Pool -----------------------------------

var bufferPools = [9]*sync.Pool{
	0: {New: func() interface{} { return make([]byte, 0, poolThreshold1+overhead) }},
	1: {New: func() interface{} { return make([]byte, 0, poolThreshold2+overhead) }},
	2: {New: func() interface{} { return make([]byte, 0, poolThreshold3+overhead) }},
	3: {New: func() interface{} { return make([]byte, 0, poolThreshold4+overhead) }},
	4: {New: func() interface{} { return make([]byte, 0, poolThreshold6+overhead) }},
	5: {New: func() interface{} { return make([]byte, 0, poolThreshold8+overhead) }},
	6: {New: func() interface{} { return make([]byte, 0, poolThreshold12) }},
	7: {New: func() interface{} { return make([]byte, 0, poolThreshold16) }},
	8: {New: func() interface{} { return make([]byte, 0, poolThreshold20) }},
}

func getPoolIndex(width, height int) int {
	capacity := width * height
	switch {
	case capacity < poolThreshold1:
		return 0
	case capacity < poolThreshold2:
		return 1
	case capacity < poolThreshold3:
		return 2
	case capacity < poolThreshold4:
		return 3
	case capacity < poolThreshold6:
		return 4
	case capacity < poolThreshold8:
		return 5
	case capacity < poolThreshold12:
		return 6
	case capacity < poolThreshold16:
		return 7
	default:
		return 8
	}
}

// ------------------- Cards ----------------------------------

var bjCards = blackjackCards{
	CardCodes: [13][4]string{
		{" 2♥ ", " 2♦ ", " 2♣ ", " 2♠ "},
		{" 3♥ ", " 3♦ ", " 3♣ ", " 3♠ "},
		{" 4♥ ", " 4♦ ", " 4♣ ", " 4♠ "},
		{" 5♥ ", " 5♦ ", " 5♣ ", " 5♠ "},
		{" 6♥ ", " 6♦ ", " 6♣ ", " 6♠ "},
		{" 7♥ ", " 7♦ ", " 7♣ ", " 7♠ "},
		{" 8♥ ", " 8♦ ", " 8♣ ", " 8♠ "},
		{" 9♥ ", " 9♦ ", " 9♣ ", " 9♠ "},
		{"10♥ ", "10♦ ", "10♣ ", "10♠ "},
		{" J♥ ", " J♦ ", " J♣ ", " J♠ "},
		{" Q♥ ", " Q♦ ", " Q♣ ", " Q♠ "},
		{" K♥ ", " K♦ ", " K♣ ", " K♠ "},
		{" A♥ ", " A♦ ", " A♣ ", " A♠ "},
	},
	StandardDeck: []card{
		{Rank: two, Suit: heart, Value: 2}, {Rank: two, Suit: diamond, Value: 2},
		{Rank: two, Suit: club, Value: 2}, {Rank: two, Suit: spade, Value: 2},
		{Rank: three, Suit: heart, Value: 3}, {Rank: three, Suit: diamond, Value: 3},
		{Rank: three, Suit: club, Value: 3}, {Rank: three, Suit: spade, Value: 3},
		{Rank: four, Suit: heart, Value: 4}, {Rank: four, Suit: diamond, Value: 4},
		{Rank: four, Suit: club, Value: 4}, {Rank: four, Suit: spade, Value: 4},
		{Rank: five, Suit: heart, Value: 5}, {Rank: five, Suit: diamond, Value: 5},
		{Rank: five, Suit: club, Value: 5}, {Rank: five, Suit: spade, Value: 5},
		{Rank: six, Suit: heart, Value: 6}, {Rank: six, Suit: diamond, Value: 6},
		{Rank: six, Suit: club, Value: 6}, {Rank: six, Suit: spade, Value: 6},
		{Rank: seven, Suit: heart, Value: 7}, {Rank: seven, Suit: diamond, Value: 7},
		{Rank: seven, Suit: club, Value: 7}, {Rank: seven, Suit: spade, Value: 7},
		{Rank: eight, Suit: heart, Value: 8}, {Rank: eight, Suit: diamond, Value: 8},
		{Rank: eight, Suit: club, Value: 8}, {Rank: eight, Suit: spade, Value: 8},
		{Rank: nine, Suit: heart, Value: 9}, {Rank: nine, Suit: diamond, Value: 9},
		{Rank: nine, Suit: club, Value: 9}, {Rank: nine, Suit: spade, Value: 9},
		{Rank: ten, Suit: heart, Value: 10}, {Rank: ten, Suit: diamond, Value: 10},
		{Rank: ten, Suit: club, Value: 10}, {Rank: ten, Suit: spade, Value: 10},
		{Rank: jack, Suit: heart, Value: 10}, {Rank: jack, Suit: diamond, Value: 10},
		{Rank: jack, Suit: club, Value: 10}, {Rank: jack, Suit: spade, Value: 10},
		{Rank: queen, Suit: heart, Value: 10}, {Rank: queen, Suit: diamond, Value: 10},
		{Rank: queen, Suit: club, Value: 10}, {Rank: queen, Suit: spade, Value: 10},
		{Rank: king, Suit: heart, Value: 10}, {Rank: king, Suit: diamond, Value: 10},
		{Rank: king, Suit: club, Value: 10}, {Rank: king, Suit: spade, Value: 10},
		{Rank: ace, Suit: heart, Value: 11}, {Rank: ace, Suit: diamond, Value: 11},
		{Rank: ace, Suit: club, Value: 11}, {Rank: ace, Suit: spade, Value: 11},
	},
}

func rankToIndex(rank string) int8 {
	switch rank {
	case two:
		return 0
	case three:
		return 1
	case four:
		return 2
	case five:
		return 3
	case six:
		return 4
	case seven:
		return 5
	case eight:
		return 6
	case nine:
		return 7
	case ten:
		return 8
	case jack:
		return 9
	case queen:
		return 10
	case king:
		return 11
	case ace:
		return 12
	default:
		return -1
	}
}

func suitToIndex(suit string) int8 {
	switch suit {
	case heart:
		return 0
	case diamond:
		return 1
	case club:
		return 2
	case spade:
		return 3
	default:
		return -1
	}
}

// ------------------- Regex ----------------------------------

var regexIntegers = regexp.MustCompile(`\d+`)

// ------------------- base45 ---------------------------------

var base45Lookup = [256]int8{
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, -1, -1, -1, -1, -1, -1,
	-1, 9, 10, 11, 12, 13, 14, 15, 16, -1, 17, 18, -1, -1, -1, -1,
	19, -1, 20, 21, 22, -1, -1, 23, 24, 25, 26, -1, -1, -1, -1, -1,
	-1, 27, 28, 29, 30, 31, 32, 33, 34, -1, 35, 36, -1, -1, -1, -1,
	37, -1, 38, 39, 40, -1, -1, 41, 42, 43, 44, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
}
