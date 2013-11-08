import Pitch.Deck
import Pitch.Card
import Pitch.Game
import Pitch.Network
import Pitch.Players
import System.Random
import Control.Monad.State
import Control.Monad
import Control.Concurrent
import System.IO
import Network


main = do
  gen <- getStdGen
  netPlayer1 <- mkNetworkPlayer "net1"
  netPlayer2 <- mkNetworkPlayer "net2"
  let gameState = mkGameState gen [Player netPlayer1, Player (Human "John"), Player (Human "Alex"), Player netPlayer2]
  runStateT playGame gameState
  