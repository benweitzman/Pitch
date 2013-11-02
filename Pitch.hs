import Deck
import System.Random

main = do
  gen <- getStdGen
  let (deck, g) = shuffleDeck gen newDeck
  putStrLn . show . deal 5 $ deck