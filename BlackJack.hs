import System.Environment
import Cards.PlayingCards as C
import Cards.BlackjackGame
import Control.Monad.State


main :: IO ()
main = do
    print "Welcome to Blackjack"
    win <- evalStateT gameLoop newGameState
    if win
        then print "You won!"
        else print "You lost!"

gameLoop :: GameState Bool
gameLoop = do
    ps <- playerGo
    liftIO $ print $ "Your score: " ++ (show ps)
    es <- dealerGo
    newHand
    liftIO $ print $ "Dealer score: " ++ (show es)
    return (es < ps)
        
playerGo:: GameState Int
playerGo = do
    liftIO $ print "Stick or Twist?"
    m <- liftIO getLine
    stop <- playerTurn (read m)
    (h, _) <- get
    liftIO $ print h
    if stop 
        then score
        else playerGo

dealerGo :: GameState Int
dealerGo = return 1
    





