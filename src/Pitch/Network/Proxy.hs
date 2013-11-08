module Pitch.Network.Proxy (runProxy)
       
where       

import Pitch.Network
import Pitch.Players
import Pitch.Game
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Network.HTTP
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T

getURI :: IO URI
getURI = do putStrLn "What server are you connecting to?"
            server <- getLine
            case parseURI server of
              Just uri -> return uri
              Nothing -> do putStrLn "Invalid server"
                            getURI
             
runProxy :: Player -> IO () 
runProxy p = do uri <- getURI
                forever $ do response <- simpleHTTP (getRequest (show uri))
                             body <- getResponseBody response
                             let parsed = decode (L.pack body) :: Maybe NetStatus
                             case parsed of 
                               Nothing -> return ()
                               Just (NetStatus messages (pgs, hand)) -> forM_ messages putStrLn
                             threadDelay 500000

