### Poker Simulation Study
### Note: not attached to any of the current code, feel free to delete/change whatever







deck = setRefClass("deck", fields=list(cards="vector"))
player = setRefClass("player", fields=list(hand="vector", money="numeric", score="numeric"))

# Function to take existing deck and randomly order all cards
shuffle_deck = function(deck) {
  # Goes through the deck card by card and swap each with a randomly selected card
  for(card in c(1:length(deck))) {
    index = ceiling(runif(1, 1, length(deck)))
    temp = deck[card]
    deck[card] = deck[index]
    deck[index] = temp
  }
  return(deck)
}

# Returns a vector of the n drawn cards from top of the provided deck
# then removes the top n cards from the deck
draw = function(deck, n = 1) {
  # Creating vector of drawn cards to return
  drawn = deck$cards[(length(deck$cards) - n + 1):length(deck$cards)]
  # Removing drawn cards
  deck$cards = deck$cards[1:(length(deck$cards) - n)]
  return(drawn)
}

# Function to initialize new vector with all cards
make_deck = function() {
  deck = c(1:51, "")
  suits = c("Clubs", "Hearts", "Spades", "Diamonds")
  ranks = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")
  count = 1
  for (rank in ranks) {
    for (suit in suits) {
      deck[count] = paste(rank, "of", suit)
      count = count + 1
    }
  }
  return(deck)
}

# Draws n cards from deck and adds those cards to all players' hands
draw_community = function(deck, n = 1) {
  cards = draw(deck, n)
  for (p in players) {
    p$hand = append(p$hand, cards)
  }
}

# Calculate the score of a players' hand
calc_score = function(p) {
  # startsWith(d$cards, "Queen")
  return(1)
}

# Increases/decreases money field of provided player object by amount
# If a list of players is passed, they're all modified by amount
update_money = Vectorize(function(p, amount) {
  p$money = p$money + amount
})

# Simulating a single hand of Texas hold'em poker
simulate_hand = function() {
  # Initialize deck and player's hands
  d = deck(cards=shuffle_deck(make_deck()))
  p1 = player(hand=draw(d,2), money=1000, score=0)
  p2 = player(hand=draw(d,2), money=1000, score=0)
  players = c(p1, p2, p3) 
  pool = 0
  
  # Initial betting
  
  # Drawing the flop
  draw_community(d, 3)
  
  # Repeat betting and reveal cycle twice more
  for (i in 1:2) {
    # Betting
    
    # Reveal next card
    draw_community(d)
  }
  
  # Final bets
  
  # Calculate hands and determine winner
  scores = vector(mode="numeric", length=length(players))
  for (p in 1:length(players)) {
    #players[[p]]$score = calc_score(players[[p]])
    scores[p] = players[[p]]$score
  }
  # Vector of winning player(s)
  winners = players[c(which(scores==(max(scores))))]
  
  # Award pool to winner
  per_player = pool / length(winners)
  update_money(winners, per_player)
}


