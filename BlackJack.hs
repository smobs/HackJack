import System.Environment
import Cards.PlayingCards as C
import Cards.BlackjackGame
import Control.Monad.State


main :: IO ()
main = do
    print "Welcome to Blackjack"
    runStateT gameLoop newGameState
    return ()

gameLoop :: GameState Int
gameLoop = do
    liftIO $ print "Stick or Twist?"
    m <- liftIO $ getLine
    stop <- playerTurn (read m)
    (h, _) <- get
    liftIO $ print h
    s <- score
    liftIO $ print s
    if stop 
        then score
        else gameLoop
        

    









