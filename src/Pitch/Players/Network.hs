{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Pitch.Players.Network
       
where       

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Writer
import Data.Aeson (ToJSON, toJSON, FromJSON, fromJSON, encode, decode, Value (..), object, (.:), (.=), parseJSON, (.:?))
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text as T
import GHC.Generics
import Pitch.Game
import Pitch.Card
import Pitch.Network
import Pitch.Network.JSON
import System.Random
  
data NetworkConfig = NetworkConfig {readChannel :: Chan String
                                   ,writeChannel :: Chan (Status, GameState)
                                   ,state :: MVar (Writer [String] (GameState, ActionRequired, Int))
                                   ,authentication :: MVar (Bool, String)
                                   ,thread :: ThreadId
                                   }          

data ActionRequired = Wait | BidAction | PlayAction deriving (Show, Generic)

instance ToJSON ActionRequired where                              
  toJSON action = String . T.pack $ case action of
                                      Wait -> "Wait"
                                      BidAction -> "Bid"
                                      PlayAction -> "Play"
  
instance FromJSON ActionRequired where
  parseJSON (String v) = case T.unpack v of 
                           "Wait" -> return Wait
                           "Bid" -> return BidAction
                           "Play" -> return PlayAction
                           _ -> mzero
  parseJSON _ = mzero                           

data NetStatus = NetStatus {messages :: [String], gamestate :: (GameState, ActionRequired, Int)} deriving (Show, Generic)
instance ToJSON NetStatus
instance FromJSON NetStatus

data Status = Success String | Failure String

instance ToJSON Status where
  toJSON (Success m) = object ["code" .= toJSON (20 :: Int), "message" .= toJSON m]
  toJSON (Failure m) = object ["code" .= toJSON (50 :: Int), "message" .= toJSON m]

instance FromJSON Status where
  parseJSON (Object v) = mkStatus <$>
                           v .: "code" <*>
                           v .: "message"
  parseJSON _ = mzero                           


mkStatus :: Int -> String -> Status
mkStatus 20 = Success
mkStatus 50 = Failure

mkNetworkConfig :: StdGen -> IO (NetworkConfig, StdGen)
mkNetworkConfig g = do rchannel <- newChan
                       wchannel <- newChan
                       state <- newEmptyMVar
                       authentication <- newMVar (False, token)
                       threadId <- forkIO $ runServer (rchannel, wchannel, state, authentication)
                       return (NetworkConfig {readChannel = rchannel
                                             ,writeChannel = wchannel
                                             ,state = state
                                             ,thread = threadId
                                             ,authentication = authentication
                                             }
                               ,g'
                               )
  where (token, g') = iterate (\(xs, g) -> let (x, g') = randomR ('A', 'Z') g in (x:xs, g')) ([], g) !! 20    
    
setAction :: MVar (Writer [String] (GameState, ActionRequired, Int)) -> ActionRequired -> IO ()
setAction mvar ar = modifyMVar_ mvar $ \w -> return (do (a, _, c) <- w
                                                        return (a, ar, c))

setGameState :: MVar (Writer [String] (GameState, ActionRequired, Int)) -> GameState -> IO ()
setGameState mvar gs = modifyMVar_ mvar $ \w -> return (do (_, b, c) <- w
                                                           return (gs, b, c))

putMessage :: MVar (Writer [String] a) -> String -> IO ()
putMessage mvar message = modifyMVar_ mvar $ \w -> return (do v <- w
                                                              tell [message]
                                                              return v)

updateMVAR :: Monoid a => MVar (Writer a b) -> b -> IO ()
updateMVAR mvar v = do mw <- tryTakeMVar mvar
                       case mw of 
                         Nothing -> putMVar mvar (writer (v, mempty))
                         Just x -> putMVar mvar (do x
                                                    return v)

mkNetworkPlayerFromConfig :: NetworkConfig -> String -> Player
mkNetworkPlayerFromConfig netc@NetworkConfig {readChannel=rchannel 
                                             ,writeChannel=wchannel
                                             ,state=state
                                             ,authentication=authentication
                                             ,thread=threadId
                                             }
                          name = player
  where player = defaultPlayer {
    initGameState = \pid gs -> updateMVAR state (gs, Wait, pid)
    ,
    mkBid = \pid gs -> 
        do setAction state BidAction
           setGameState state gs
           string <- readChan rchannel
           let maybeBid = decode (pack string) :: Maybe (Int, Maybe Suit)
           bid <- case maybeBid of
                    Just x -> return x
                    Nothing -> do writeChan wchannel (Failure "Parser error", gs)
                                  (mkBid player) pid gs
           validatedBid <- case validateBid pid gs bid of 
                            Nothing -> return bid
                            Just errorMessage -> do writeChan wchannel (Failure errorMessage, gs)
                                                    (mkBid player) pid gs
           writeChan wchannel (Success "Bid accepted", gs)
           setAction state Wait
           return validatedBid                   
    ,
    mkPlay = \pid gs ->
       do setAction state PlayAction
          setGameState state gs
          string <- readChan rchannel
          let maybeCard = decode (pack string) :: Maybe Card
          card <- case maybeCard of 
                    Just x -> return x
                    Nothing -> do writeChan wchannel (Failure "Parser error", gs)
                                  (mkPlay player) pid gs
          validatedCard <- case validateCard pid gs card of
                             Nothing -> return card
                             Just errorMessage -> do writeChan wchannel (Failure errorMessage, gs)
                                                     (mkPlay player) pid gs
          writeChan wchannel (Success "Card accepted", gs)
          setAction state Wait
          return validatedCard                                             
    ,
    -- postBidHook :: (p, Int) -> Bid -> GameState -> IO ()
    postBidHook = \pid
                   Bid {amount=amount,bidSuit=suit,bidderIdx=bidderIdx} 
                   gs@(Game scores players rounds hand) 
                   -> 
      do let bidMessage = case amount of
                           0 -> players !! bidderIdx ++ " passed"
                           x -> players !! bidderIdx ++ " bid " ++ show x
         putMessage state bidMessage
         setGameState state gs
    ,
    --  postPlayHook :: (p, Int) -> Play -> GameState -> [Card] -> IO ()                       
    postPlayHook = \pid
                    Play {card=card,playerIdx=playerIdx}
                    gs@(Game scores players rounds hand)
                    ->  
      do let playMessage = players !! playerIdx ++ " played " ++ show card
         putMessage state playMessage
         setGameState state gs
    ,
    acknowledgeTrump = \pid
                        Bid {amount=amount,bidSuit=Just suit,bidderIdx=bidderIdx} 
                        _
                        ->
      putMessage state $ "Trump is " ++ show suit
   ,
   name = name
}

mkNetworkPlayer :: StdGen -> String -> IO (Player, StdGen)
mkNetworkPlayer g name = do (conf, g) <- mkNetworkConfig g
                            let p = mkNetworkPlayerFromConfig conf name
                            return (p, g)