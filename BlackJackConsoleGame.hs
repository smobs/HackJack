import System.Environment
import Cards.BlackjackGameState
import Control.Monad.State

type Player = String

main :: IO ()
main = do
    welcome
    startState <- initialise
    win <- evalStateT gameLoop startState
    victory win

gameLoop :: GameState Bool
gameLoop = do
    drawNewPlayerHand
    ps <- playerGo
    showScore "Your" ps
    drawNewPlayerHand
    es <- dealerGo ps
    showScore "Dealer" es
    return (es < ps)
   
turn :: GameState Move -> GameState HandScore
turn getMove = do
    printHand
    m <- getMove
    stop <- playerTurn m
    if stop 
        then score
        else turn getMove
        
playerGo :: GameState HandScore
playerGo = turn playerControl

dealerGo :: HandScore -> GameState HandScore
dealerGo = turn.dealerControl

playerControl :: GameState Move
playerControl = do
    liftIO $ print "Stick or Twist?"
    m <- liftIO getLine
    return (read m)
   
dealerControl :: HandScore -> GameState Move
dealerControl target = do
    s <- score
    return (if s < target then Twist else Stick)
    
showScore :: Player -> HandScore -> GameState ()
showScore p = liftIO.print.( (p ++ " score: ") ++).show

welcome :: IO ()
welcome = print "Welcome to Blackjack"

victory :: Bool -> IO()
victory win = print $ if win then  "You won!" else "You lost!"
