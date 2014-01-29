module Cards.BlackjackGameState(
Move(Stick, Twist),
playerTurn,
drawNewPlayerHand,
GameState,
score,
initialise,
HandScore
)
where
import Control.Applicative
import Control.Monad.State
import Cards.Blackjack

import System.Random
import System.Random.Shuffle

type GameState a = StateT (Hand, DeckOfCards) IO a

data Move = Stick | Twist deriving (Eq, Read)

initialise :: IO (Hand , DeckOfCards)
initialise = do
    sdeck <- shuffleM blackjackDeck
    return ([], sdeck)

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
    
drawNewPlayerHand :: GameState ()
drawNewPlayerHand = emptyHand

emptyHand :: GameState ()
emptyHand = do
    (_ , deck) <- get
    (put.newHand) deck

playerTurn :: Move -> GameState Bool
playerTurn Stick = return True
playerTurn Twist = do
    draw
    s <- score
    case s of 
        Score _ -> return False
        _ -> return True
