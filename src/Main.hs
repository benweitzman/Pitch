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
  netPlayer <- mkNetworkPlayer "net"
  let gameState = mkGameState gen [Player (Human "Ben"), Player (Human "John"), Player (Human "Alex"), Player netPlayer]
  runStateT playGame gameState
  