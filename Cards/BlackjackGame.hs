module Cards.BlackjackGame(
Move(Stick, Twist),
playerTurn,
newGameState,
newHand,
GameState,
score
)
 where
import Cards.PlayingCards
import Data.Maybe
import Control.Applicative
import Control.Monad.State

type HandValue = [Int]
type GameState a = StateT (HandValue, DeckOfCards) IO a

data Move = Stick | Twist deriving (Eq, Read)

newGameState :: (HandValue, DeckOfCards)
newGameState = ([0],  deckOfCards)

newHand :: GameState ()
newHand = do
    (h, d) <- get
    put ([0], d)

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
validHands = filter (21 >=)

calcScore :: HandValue -> Int
calcScore = foldr max  0.validHands

mapHands ::  (HandValue -> a) -> GameState a
mapHands f = do
    (h, _) <- get
    return (f h)

score ::  GameState Int
score = mapHands calcScore

draw :: GameState ()
draw = do
    (h, d:ds) <- get
    liftIO $ print $ "Drawn the " ++ show d
    put ( addCardToHand (cardValue d) h,ds)

playerTurn :: Move -> GameState Bool
playerTurn Stick = return True
playerTurn Twist = do
    draw
    mapHands bust
