import System.Environment

main :: IO ()
main = getArgs >>= print.(\x -> "Hello " ++ x) .head

