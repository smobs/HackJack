module Cards.PlayingCards where

data Suit = Hearts | Diamonds | Clubs | Spades deriving Show

data CardValue = Ace | King | Queen | Jack | CardValue Int 

data Card = Card Suit CardValue

instance Show Card where
    show (Card s cv) = show cv ++ " of " ++ show s
    
instance Show CardValue where
    show (CardValue v) = show v
    show Ace = "Ace"
    show King = "King"
    show Queen = "Queen"
    show Jack = "Jack"
    
type DeckOfCards = [Card]

suits :: [Suit]
suits = [Hearts, Diamonds, Clubs, Spades]

cardValues :: [CardValue]
cardValues = [Ace, King, Queen, Jack] ++ (map CardValue [2..10] )

deckOfCards :: DeckOfCards
deckOfCards = concatMap (\x -> map (\f -> f x)(map Card suits)) cardValues