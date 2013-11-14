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
  netPlayer3 <- mkNetworkPlayer "net3"
  let gameState = mkGameState gen [Player (Human "John"), Player netPlayer1, Player netPlayer2, Player netPlayer3]
  runStateT playGame gameState
  