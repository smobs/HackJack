module Cards.Blackjack(
computeHandValue,
Hand,
DeckOfCards,
HandScore(Score, Bust),
newHand,
blackjackDeck
)
where
import Cards.PlayingCards
import Data.Maybe

type Hand = [Card]
data HandScore = Bust | Score Int | BlackJack deriving (Ord, Show, Eq)

newHand :: DeckOfCards -> (Hand, DeckOfCards)
newHand d = ([], d)

blackjackDeck :: DeckOfCards
blackjackDeck = deckOfCards

isBlackjack :: Hand -> Bool
isBlackjack (c1:c2:[]) = (value Ace c1 && isRoyal c2) || (value Ace c2 && isRoyal c1)
isBlackjack _ = False

computeHandValue :: Hand -> HandScore
computeHandValue h = if isBlackjack h then BlackJack else getHandScore $ computeHandValue' h

computeHandValue' :: Hand ->  [Int]
computeHandValue' [] = [0]
computeHandValue' [i] = cardValue i
computeHandValue' (x:xs) = let cv = computeHandValue' xs in
       cardValue x >>= (\y -> map(y+) cv)

getHandScore :: [Int] -> HandScore
getHandScore [] = Score 0
getHandScore ss = if null v then Bust else Score (maximum v)
    where v = filter (21 >=) ss

cardValue :: Card -> [Int]
cardValue (Card _ Ace) = [1, 11]
cardValue (Card _ x) = maybeToList $ lookup x $ zip [Two .. King] ([2..10] ++ repeat 10) 
