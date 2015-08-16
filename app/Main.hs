import Prelude    hiding (lex)
import Discussion

main :: IO()
main = do
  let lexed  = lex "y = ``x (`y z) `(\\x -> `x x) w"
  putStrLn $ show lexed
  putStrLn ""
  let parsed = parse =<< lexed
  putStrLn $ show parsed
