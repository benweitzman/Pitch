import Pitch.Players
import Pitch.Game
import Pitch.Network.Proxy

main = runProxy (mkPlayer "net")