{-# LANGUAGE DeriveGeneric #-}

module Pitch.Internal.PartialGame (RoundState (..)
                                  ,GameState (..)
                                  ,Player (..)
                                  ,defaultPlayer
                                  ) 
where

import GHC.Generics
import Pitch.Internal.Common
import Pitch.Card

data RoundState = Round { bids :: [Bid] 
                        , trump :: Suit 
                        , tricks :: [Trick] 
                        } deriving (Show, Generic)
data GameState = Game { scores :: [(PlayerId, Int)] 
                      , players :: [String] 
                      , rounds :: [RoundState]  
                      , hand :: [Card]
                      } deriving (Show, Generic)

data Player = Player { initGameState :: PlayerId -> GameState -> IO ()
                     , mkBid :: PlayerId -> GameState -> IO (Int, Maybe Suit)
                     , mkPlay :: PlayerId -> GameState -> IO Card
                     , postBidHook :: PlayerId -> Bid -> GameState -> IO ()
                     , postPlayHook :: PlayerId -> Play -> GameState -> IO ()
                     , acknowledgeTrump :: PlayerId -> Bid -> GameState  -> IO ()
                     , name :: String
                     } 
              
defaultPlayer :: Player              
defaultPlayer = Player {initGameState = \_ _ -> return ()
                       ,mkBid = undefined
                       ,mkPlay = undefined
                       ,postBidHook = \_ _ _ -> return ()
                       ,postPlayHook = \_ _ _ -> return ()
                       ,acknowledgeTrump = \_ _ _ -> return ()
                       ,name = undefined
                       } 
              
instance Show Player where
  show = name