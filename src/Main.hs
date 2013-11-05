import Pitch.Deck
import Pitch.Card
import Pitch.Game
import Pitch.Network
import System.Random
import Control.Monad.State
import Control.Monad
import Control.Concurrent
import System.IO
import Network

main = do
  serverThreadID <- forkIO runServer
  gen <- getStdGen
  let gameState = mkGameState gen [Player (Human "Ben"), Player (Human "John"), Player (Human "Alex")]
  runStateT playGame gameState
  