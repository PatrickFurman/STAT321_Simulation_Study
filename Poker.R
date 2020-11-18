### Poker Simulation Study
### Note: not attached to any of the current code, feel free to delete/change whatever







deck = setRefClass("deck", fields=list(cards="vector"))
player = setRefClass("player", fields=list(hand="vector", money="numeric",
                                           score="numeric", high_card="numeric"))

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

# Draws n cards from deck for each player and adds to their hands
deal_all = function(deck, n = 1) {
  for (p in players) {
    p$hand = append(p$hand, draw(deck,n))
  }
}

# Helper function for calc_score that uses a vector of how many cards of each rank
# are in a hand to determine if those cards form a straight
contains_straight = function(ranks) {
  has_rank = which(ranks > 0)
  temp = has_rank[1]
  in_a_row = 1
  for (i in 2:length(has_rank)) {
    temp2 = has_rank[i]
    if (temp2 == (temp + 1)) {
      in_a_row = in_a_row + 1
      if (in_a_row == 5) {
        # Straight
        return(TRUE)
      }
    } else {
      in_a_row = 1
    }
    temp = has_rank[i]
  }
  return(FALSE)
}

get_straight_hand = function(ranks) {
  straight_hand_ranks = ranks
  consecutive = 0
  for (i in 1:(length(straight_hand_ranks) - 1)) {
    if (straight_hand_ranks[i] != 0) {
      consecutive = consecutive + 1
      if (straight_hand_ranks[i+1] == 0 && consecutive < 4) {
        straight_hand_ranks[i] = 0
      }
    }
  }
  return(straight_hand_ranks)
}

get_flush_hand = function(full_hand, num_of_each_suit, suits, ranks) {
  flush_suit = suits[which(num_of_each_suit == max(num_of_each_suit))]
  flush_hand = c()
  for (card in full_hand) {
    if (endsWith(card, flush_suit)) {
      flush_hand = append(flush_hand, card)
    }
  }
  flush_hand_ranks = rep(0, 13)
  j = 1
  # Calculate number of each rank in flush_hand
  for (rank in ranks) {
    flush_hand_ranks[j] = sum(startsWith(flush_hand, rank))
    j = j + 1
  }
  return(flush_hand_ranks)
}

# Calculate the score of a players' hand
# Score is returned as a two element vector with hand score and high card
# Known issues:
# 1) Straight flush and rare flush are being drawn less often than theoretical
#    probabilities suggest they should be
# 2) Doesn't ensure that the chosen high card is one of the 5 cards that make up the best
#    possible hand (only important for straights/flushes/other five card hands)
calc_score = function(p) {
  suits = c("Clubs", "Hearts", "Spades", "Diamonds")
  ranks = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")
  highest = 0
  straight = FALSE
  num_of_each_rank = rep(0, 13)
  num_of_each_suit = rep(0, 4)
  i = 1
  # Calculate number of each rank in hand for pairs/3 of a kind etc.
  for (rank in ranks) {
    num_of_each_rank[i] = sum(startsWith(p$hand, rank))
    i = i + 1
  }
  i = 1
  # Calculate number of each suit in hand for flush
  for (suit in suits) {
    num_of_each_suit[i] = sum(endsWith(p$hand, suit))
    i = i + 1
  }
  # First scoring by rank because highest possible hands are dependent on suit
  # so they can overwrite later if needed
  of_a_kind = max(num_of_each_rank)
  if (of_a_kind < 4) {
    if (of_a_kind == 3) {
      if (2 %in% num_of_each_rank | length(which(num_of_each_rank == 3)) == 2) {
        # Full house
        rank_of_hand = max(which(num_of_each_rank %in% c(2,3))) + 1
        highest = 84 + rank_of_hand
      } else {
        # Three of a kind
        rank_of_three = max(which(num_of_each_rank == 3)) + 1
        highest = 42 + rank_of_three
      }
    } else if (of_a_kind == 2) {
      if (sum(2==num_of_each_rank) >= 2) {
        # Two pair
        rank_of_pair = max(which(num_of_each_rank == 2)) + 1
        highest = 28 + rank_of_pair
      } else {
        # One pair
        rank_of_pair = max(which(num_of_each_rank == 2)) + 1
        highest = 14 + rank_of_pair
      }
    }
  } else {
    # 4 of a kind
    rank_of_four = max(which(num_of_each_rank == 4)) + 1
    highest = 98 + rank_of_four
  }
  # Checking for straight
  if (contains_straight(num_of_each_rank)) {
    straight_hand_ranks = get_straight_hand(num_of_each_rank)
    flush_hand_ranks = rep(0, 13)
    if (max(num_of_each_suit) >= 5) {
      flush_hand_ranks = get_flush_hand(p$hand, num_of_each_suit, suits, ranks)
    }
    # Checking if straight also contains flush
    if (sum(straight_hand_ranks == flush_hand_ranks) == 13) {
      if (straight_hand_ranks[13] == 1) {
        # Royal Flush
        highest=127
      } else {
        # Straight Flush
        rank_of_flush = max(which(straight_hand_ranks > 0)) + 1
        highest = 112 + rank_of_flush
      }
    } else if (highest < 57) {
      # Straight
      straight_rank = max(which(straight_hand_ranks > 0)) + 1
      highest = 56 + straight_rank
    }
  }
  # Scoring again looking at suits to check for flushes
  if (max(num_of_each_suit) >= 5) {
    if (highest < 71) {
      # Regular flush
      flush_hand_ranks = get_flush_hand(p$hand, num_of_each_suit, suits, ranks)
      rank_of_flush = max(which(flush_hand_ranks == 1)) + 1
      highest = 70 + rank_of_flush
    }
  }
  # Find high card to return with hand score for ties
  high_card = max(which(num_of_each_rank > 0)) + 1
  p$score = highest
  if (highest == 0) {
    p$score=high_card
  } else {
    p$score=highest
  }
  p$high_card=high_card
}

# Increases/decreases money field of provided player object by amount
# If a list of players is passed, they're all modified by amount
# NOTE: must pass as players[i] not just p1 - will return an error (not sure why)
update_money = Vectorize(function(p, amount) {
  p$money = p$money + amount
  return(p$money)
})

# Setting up players (went with 7 to avoid dealing with situation where
# it's possible to run out of cards)
p1 = player(hand=vector(), money=1000, score=0, high_card=0)
p2 = player(hand=vector(), money=1000, score=0, high_card=0)
p3 = player(hand=vector(), money=1000, score=0, high_card=0)
p4 = player(hand=vector(), money=1000, score=0, high_card=0)
p5 = player(hand=vector(), money=1000, score=0, high_card=0)
p6 = player(hand=vector(), money=1000, score=0, high_card=0)
p7 = player(hand=vector(), money=1000, score=0, high_card=0)
players = c(p1, p2, p3, p4, p5, p6, p7) 

# Simulating a single hand of seven-card stud poker
simulate_hand = function() {
  # Initialize deck and player's hands
  d = deck(cards=shuffle_deck(make_deck()))
  for (p in players) {
    p$score = 0 # Reset score after each round
    p$hand = vector() # Reset hand after each round
  }
  deal_all(d,3) # Deal two face down cards plus one face up to each player
  pot = 100
  
  # Initial betting - starting with lowest valued shown card
  
  # Repeat betting and dealing cycle 3 times (third-sixth streets)
  for (i in 1:3) {
    # Scoring? (to inform bets)
    
    # Betting - starting with highest valued shown hand
    
    # Dealing everyone another card
    deal_all(d)
  }
  
  # Final face up card (seventh street)
  deal_all(d)
  
  # Final bets
  
  # Calculate hands and determine winner
  scores = vector(mode="numeric", length=length(players))
  high_cards = vector(mode="numeric", length=length(players))
  for (p in 1:length(players)) {
    calc_score(players[[p]])
    high_cards[p] = players[[p]]$high_card
    scores[p] = players[[p]]$score
  }
  # Vector of winning player(s)
  ties = which(scores==(max(scores)))
  winners = players[c(ties[which(high_cards[c(ties)]==(max(high_cards[c(ties)])))])]
  
  # Award pot to winner(s)
  per_player = pot / length(winners)
  update_money(winners, per_player)
}

# Frequency of Each Type of Hand ------------------------------------------

# Creates 70,000 hands, scores them and returns results in a vector
f = function(x) {
  deck = setRefClass("deck", fields=list(cards="vector"))
  player = setRefClass("player", fields=list(hand="vector", money="numeric",
                                             score="numeric", high_card="numeric"))
  
  recorded_scores = rep(0,127)
  for(i in 1:10000) {
    p1 = player(hand=vector(), money=1000, score=0, high_card=0)
    p2 = player(hand=vector(), money=1000, score=0, high_card=0)
    p3 = player(hand=vector(), money=1000, score=0, high_card=0)
    p4 = player(hand=vector(), money=1000, score=0, high_card=0)
    p5 = player(hand=vector(), money=1000, score=0, high_card=0)
    p6 = player(hand=vector(), money=1000, score=0, high_card=0)
    p7 = player(hand=vector(), money=1000, score=0, high_card=0)
    players = c(p1, p2, p3, p4, p5, p6, p7) 
    d = deck(cards=shuffle_deck(make_deck()))
    for (p in players) {
      p$hand = draw(d,7)
      p$score = 0
      calc_score(p)
      recorded_scores[p$score] = recorded_scores[p$score] + 1
    }
  }
  return(recorded_scores)
}

library(foreach)
library(doParallel)
num_cores = detectCores()
cl = makeCluster(num_cores - 2)
registerDoParallel(cl)

start = Sys.time()
# Currently will process 70,000 * length(x) hands - To change, alter i or players vector in f(x)
recorded_scores = foreach(x = 1:10, .combine='+') %dopar% f(x)
end = Sys.time()
end-start

recorded_hands = c("0"=0, "1"=0, "2"=0, "3"=0, "4"=0, "5"=0, "6"=0, "7"=0, "8"=0, "9"=0)
for (i in 0:8) {
  recorded_hands[i+1] = sum(recorded_scores[(i*14+1):((i+1)*14)])
}
recorded_hands[10] = recorded_scores[127]
# Taking combined vector of scores from above and making a dataframe with percentage
# probability of getting each type of hand
df = data.frame(Percent_of_Hands=recorded_hands[c(1:10)] / sum(recorded_hands) * 100,
                row.names = c("High Card", "Pair", "2 Pair", "3 of a Kind", "Straight",
                              "Flush", "Full House", "4 of a Kind", "Straight Flush", "Royal Flush"))
format(df, scientific=FALSE)


# Probability of Winning Based on Cards in the Hole -----------------------




# Betting High vs. Betting Low --------------------------------------------


