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
import Control.Monad.State

type HandValue = [Int]
type GameState = (HandValue, DeckOfCards)


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

draw :: State GameState ()
draw = do
    (h, d:ds) <- get
    put ( addCardToHand (cardValue d) h,ds)

move :: Move -> State GameState ()
move Twist = draw
move _ = return ()