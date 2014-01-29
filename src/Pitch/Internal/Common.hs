{-# LANGUAGE DeriveGeneric #-}
module Pitch.Internal.Common where

import GHC.Generics
import Pitch.Card

type PlayerId = Int

data Hand = Hand {cards :: [Card], ownerIdx :: PlayerId} deriving (Show, Eq, Generic)
data Bid = Bid {amount :: Int, bidSuit :: Maybe Suit, bidderIdx :: PlayerId} deriving (Show, Eq, Generic)
data Play = Play {card :: Card, playerIdx :: PlayerId} deriving (Show, Eq, Generic)
data Trick = Trick {played :: [Play], winnerIdx :: PlayerId} deriving (Show, Eq, Generic)