module Cards.BlackjackGame(
Move(Stick, Twist),
playerTurn,
newHand,
GameState,
HandScore,
score
)
 where
import Cards.PlayingCards
import Data.Maybe
import Control.Applicative
import Control.Monad.State

type Hand = [Card]
data HandScore = Bust | Score Int | BlackJack deriving (Ord, Show, Eq)

type GameState a = StateT (Hand, DeckOfCards) IO a

data Move = Stick | Twist deriving (Eq, Read)

newHand :: DeckOfCards -> (Hand, DeckOfCards)
newHand d = ([], d)

isBlackjack :: Hand -> Bool
isBlackjack (c1:c2:[]) = (value Ace c1 && isRoyal c2) || (value Ace c2 && isRoyal c1)
isBlackjack _ = False

computeHandValue :: Hand -> HandScore
computeHandValue h = if isBlackjack h then BlackJack else getHandScore $ computeHandValue' h

computeHandValue' :: Hand ->  [Int]
computeHandValue' [] = [0]
computeHandValue' [i] = cardValue i
computeHandValue' (x:xs) = let cv = computeHandValue' xs in
      cardValue x >>= (: cv)

getHandScore :: [Int] -> HandScore
getHandScore [] = Score 0
getHandScore ss = if null v then Bust else Score (maximum v)
    where v = filter (21 >=) ss

cardValue :: Card -> [Int]
cardValue (Card _ Ace) = [1, 11]
cardValue (Card _ x) = maybeToList $ lookup x $ zip [Two .. King] ([2..10] ++ repeat 10) 


mapHands ::  (Hand -> a) -> GameState a
mapHands f = do
    (h, _) <- get
    return (f h)

score :: GameState HandScore
score = mapHands computeHandValue
        

draw :: GameState ()
draw = do
    (h, d:ds) <- get
    liftIO $ print $ "Drawn the " ++ show d
    put ( d:h,ds)

playerTurn :: Move -> GameState Bool
playerTurn Stick = return True
playerTurn Twist = do
    draw
    s <- score
    case s of 
        Score _ -> return False
        _ -> return True
