module Pitch.Network.Proxy (runProxy)
       
where       

import Pitch.Network
import Pitch.Players
import Pitch.Game
import Pitch.Card
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Network.HTTP
import Network.URI
import Network.HTTP.Base
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T

getURI :: IO URI
getURI = do putStrLn "What server are you connecting to?"
            server <- getLine
            case parseURI server of
              Just uri -> return uri
              Nothing -> do putStrLn "Invalid server"
                            getURI

mkPostRequest :: URI -> String -> Request String
mkPostRequest uri body =  Request { rqURI     = uri
                                  , rqMethod  = POST
                                  , rqHeaders = [ mkHeader HdrContentType "application/x-www-form-urlencoded"
                                                , mkHeader HdrContentLength (show (length body))
                                                ]
                                  , rqBody    = body
                                  }         

postJSON :: ToJSON a => URI -> a -> IO ()
postJSON uri obj =  let responseJSON = L.unpack $ encode obj
                        pRequest = mkPostRequest uri responseJSON
                    in do response <- simpleHTTP pRequest
                          body <- getResponseBody response                               
                          let parsed = decode (L.pack body) :: Maybe (Status, PartialGameState, [Card])
                          case parsed of    
                            Just (Failure m, _, _) -> putStrLn $ "Server says: " ++ m
                            _ -> return ()
             
runProxy :: Player -> IO () 
runProxy (Player p) = do uri <- getURI
                         authResponse <- simpleHTTP . getRequest $ show uri
                         key <- getResponseBody authResponse
                         let uri' = uri{uriPath = '/' : urlEncode (init key)}
                         forever $ do response <- simpleHTTP (getRequest (show uri'))
                                      body <- getResponseBody response
                                      let parsed = decode (L.pack body) :: Maybe NetStatus
                                      case parsed of 
                                        Nothing -> return ()
                                        Just (NetStatus messages (pgs, hand, action, idx)) -> 
                                           do forM_ messages putStrLn
                                              case action of 
                                               Wait -> return ()
                                               BidAction -> do (amount, msuit) <- mkBid (p, idx) pgs hand
                                                               postJSON uri' (amount, msuit) 
                                               PlayAction -> do card <- mkPlay (p, idx) pgs hand
                                                                postJSON uri' card
                                      threadDelay 500000

