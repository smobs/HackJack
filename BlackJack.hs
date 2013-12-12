import System.Environment
import Cards.PlayingCards as C
import Cards.BlackjackGame
import Control.Monad.State


main :: IO ()
main = do 
    runStateT gameLoop newGameState
    return ()

gameLoop :: GameState Int
gameLoop = do
    m <- liftIO $ getLine
    stop <- playerTurn (read m)
    (h, _) <- get
    liftIO $ print  h
    if stop 
        then score
        else gameLoop
        

    









