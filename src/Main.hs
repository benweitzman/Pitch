import Pitch.Deck
import Pitch.Card
import Pitch.Game
import System.Random
import Control.Monad.State
import Control.Monad

main = do
  gen <- getStdGen
  let gameState = mkGameState gen [Player (Human "Ben"), Player (Human "John"), Player (Human "Alex")]
  runStateT playGame gameState
