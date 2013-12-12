import System.Environment
import Cards.PlayingCards as C
import Cards.BlackjackGame
import Control.Monad.State

import System.Random
import System.Random.Shuffle

type Player = String

main :: IO ()
main = do
    deck <- shuffleM C.deckOfCards 
    welcome
    win <- evalStateT gameLoop (newHand deck)
    victory win

gameLoop :: GameState Bool
gameLoop = do
    ps <- playerGo
    showScore "Your" ps
    (_, deck) <- get
    put (newHand deck)
    es <- dealerGo ps
    showScore "Dealer" ps
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
    
showScore :: Player -> Int -> GameState ()
showScore p = liftIO.print.( (p ++ " score: ") ++).show

welcome :: IO ()
welcome = print "Welcome to Blackjack"

victory :: Bool -> IO()
victory win = print $ if win then  "You won!" else "You lost!"
