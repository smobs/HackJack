import System.Environment
import Cards.PlayingCards as C
import Cards.BlackjackGame



main :: IO ()
main = do
    let (h ,d) = move Twist C.deckOfCards []
    print h

gameLoop :: IO()
gameLoop = do
    m <- getLine
    print m
    







