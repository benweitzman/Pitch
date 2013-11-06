{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Pitch.Players (NetworkPlayer (..)
                     ,mkNetworkPlayer
                     )

where

import Pitch.Game
import Pitch.Network
import Pitch.Parser
import Pitch.Card
import Control.Concurrent
import Control.Concurrent.Chan
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

buildBid :: String -> String -> (Int, Suit)
buildBid _ _ = (2, Clubs)

instance FromJSON (Int, Suit) where
    parseJSON (Object v) = buildBid <$>
                            v .: "amount" <*>
                            v .: "suit"
     -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

instance PlayerLogic NetworkPlayer where
    mkBid p@(NetworkPlayer {readChannel = channel}, idx) gs = 
        do string <- readChan channel
           let maybeBid = decode (pack string) :: Maybe (Int, Suit)
           bid <- case maybeBid of
                    Just x -> return x
                    Nothing -> mkBid p gs
           return bid

    mkPlay = error "Not implemented"

