module Cards.BlackjackGame(
Move(Stick, Twist),
bust,
score,
move,
newHand
)
 where
import Cards.PlayingCards
import Data.Maybe
import Control.Applicative

type HandValue = [Int]

data Move = Stick | Twist deriving (Eq, Read)

newHand :: HandValue
newHand = [0]

addCardToHand :: [Int] -> HandValue -> HandValue
addCardToHand [] = id
addCardToHand [i] = map (i + ) 
addCardToHand (x:xs) = (++) <$> map (x +) <*> addCardToHand xs

cardValue :: Card -> [Int]
cardValue (Card _ Ace) = [1, 11]
cardValue (Card _ x) = maybeToList $ lookup x $ zip [Two .. King] ([2..10] ++ repeat 10) 

bust :: HandValue -> Bool
bust = null.validHands

validHands :: HandValue -> HandValue
validHands = filter (21 <)

score :: HandValue -> Int
score = maximum.validHands

move :: Move -> (HandValue, DeckOfCards) -> (HandValue, DeckOfCards)
move Twist (h, (c:ds))= (addCardToHand (cardValue c) h , ds)
move _ (h,d) = (h,d) 