{-# LANGUAGE OverloadedStrings #-}

module Pitch.Players (NetworkPlayer (..)
                     ,mkNetworkPlayer
                     ,BidResponse (..)
                     )

where

import Pitch.Game
import Pitch.Network
import Pitch.Parser
import Pitch.Card
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Aeson.Parser
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.ByteString.Lazy.Char8 (pack)

data NetworkPlayer = NetworkPlayer {readChannel :: Chan String
                                   ,thread :: ThreadId
                                   ,name :: String
                                   }

mkNetworkPlayer :: String -> IO NetworkPlayer
mkNetworkPlayer n = do channel <- newChan
                       threadId <- forkIO $ runServer channel
                       return NetworkPlayer {readChannel = channel
                                            ,thread = threadId
                                            ,name = n
                                            }

instance Show NetworkPlayer where
    show NetworkPlayer {name = n} = n

data BidResponse = BidResponse { amount :: Int
                               , suit :: String
                               } deriving (Show)

instance FromJSON BidResponse where
    parseJSON (Object v) = BidResponse <$>
                            v .: "amount" <*>
                            v .: "suit"
     -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance PlayerLogic NetworkPlayer where
    mkBid p@(NetworkPlayer {readChannel = channel}, idx) gs = 
        do string <- readChan channel
           let maybeBid = decode (pack string) :: Maybe BidResponse
           (amount, suitString) <- case maybeBid of
                                        Just x -> return (amount x, Pitch.Players.suit x)
                                        Nothing -> do (f, s) <- mkBid p gs
                                                      return (f, show s)
           return (0, Hearts)

    mkPlay = error "Not implemented"

