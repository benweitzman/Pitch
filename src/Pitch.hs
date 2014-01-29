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
  (netPlayer1, g') <- mkNetworkPlayer gen "net1"
  (netPlayer2, g'') <- mkNetworkPlayer g' "net2"
  (netPlayer3, g''') <- mkNetworkPlayer g'' "net3" 
  let gameState = mkGameState g''' [mkPlayer  "John", netPlayer1, netPlayer2, netPlayer3]
  runStateT playGame gameState
  