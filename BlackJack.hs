import System.Environment
import Cards.PlayingCards as Cards

main :: IO ()
main = (print.unlines) $ map show Cards.deckOfCards

