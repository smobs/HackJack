module Cards.PlayingCards where

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show, Enum)

data CardValue = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Eq,Show, Enum)

    
type DeckOfCards = [Card]

data Card = Card Suit CardValue deriving Eq

instance Show Card where
    show (Card s v) = show v ++ " of " ++ show s

deckOfCards :: DeckOfCards
deckOfCards = [Card s v | s <- [Hearts .. Spades], v <- [Ace .. King]]