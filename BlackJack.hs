import System.Environment
import Cards.PlayingCards as C
import Cards.BlackjackGame
import Control.Monad.State


main :: IO ()
main = do
    print "Welcome to Blackjack"
    win <- evalStateT gameLoop newGameState
    print $ if win then  "You won!" else "You lost!"

gameLoop :: GameState Bool
gameLoop = do
    ps <- playerGo
    liftIO $ print $ "Your score: " ++ show ps
    newHand
    es <- dealerGo ps
    liftIO $ print $ "Dealer score: " ++ show es
    return (es < ps)
   
turn :: GameState Move -> GameState Int
turn getMove = do
    m <- getMove
    stop <- playerTurn m
    (h, _) <- get
    liftIO $ print h
    if stop 
        then score
        else turn getMove
        
playerGo :: GameState Int
playerGo = turn playerControl

dealerGo :: Int -> GameState Int
dealerGo = turn.dealerControl

playerControl :: GameState Move
playerControl = do
    liftIO $ print "Stick or Twist?"
    m <- liftIO getLine
    return (read m)
   
dealerControl :: Int -> GameState Move
dealerControl target = do
    s <- score
    return (if s < target then Twist else Stick)
    
    





