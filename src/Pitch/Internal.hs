module Pitch.Internal where

import           Pitch.Card
import           Pitch.Deck
import           System.Random
import Control.Monad.State

type PlayerId = Int

data Hand = Hand {cards :: [Card], ownerIdx :: PlayerId} deriving (Show, Eq)
data Bid = Bid {amount :: Int, bidSuit :: Maybe Suit, bidderIdx :: PlayerId} deriving (Show, Eq)
data Play = Play {card :: Card, playerIdx :: PlayerId} deriving (Show, Eq)
data Trick = Trick {played :: [Play], winnerIdx :: PlayerId} deriving (Show, Eq)

data GameState = Game { players   :: [(PlayerId, Player)]
                      , scores    :: [(PlayerId, Int)]
                      , rounds    :: [RoundState]
                      , dealers   :: [(PlayerId, Player)]
                      , generator :: StdGen
                      }

data RoundState = Round { deck   :: Deck
                        , hands  :: [Hand]
                        , bids   :: [Bid]
                        , trump  :: Suit
                        , tricks :: [Trick]
                        } deriving (Show)

data PartialRoundState = PartialRound [Bid] Suit [Trick] deriving (Show)
data PartialGameState = PartialGame [(PlayerId, Int)] [String] [PartialRoundState]  deriving (Show)

type Pitch = StateT GameState IO 

{-
class (Show p) => PlayerLogic p where
    initGameState :: (p, Int) -> PartialGameState -> [Card] -> IO ()
    initGameState _ _ _ = return ()

    --  The result of the state is the bid and the suit that the player will choose
    -- if they win the bid
    mkBid :: (p, Int) -> PartialGameState -> [Card] -> IO (Int, Maybe Suit)

    mkPlay :: (p, Int) -> PartialGameState -> [Card] -> IO Card
    
    -- post*Hooks will be called on each player after any player takes some action
    -- The default implementation is just to do nothing
    postBidHook :: (p, Int) -> Bid -> PartialGameState -> [Card] -> IO ()
    postBidHook _ _ _ _ = return () 
    
    postPlayHook :: (p, Int) -> Play -> PartialGameState -> [Card] -> IO ()
    postPlayHook _ _ _ _= return ()

    -- called on each player after trump has been decided
    acknowledgeTrump :: (p, Int) -> Bid -> PartialGameState -> [Card] -> IO ()
    acknowledgeTrump _ _ _ _ = return ()
-}

data Player = Player { initGameState :: PlayerId -> PartialGameState -> IO ()
                       , mkBid :: PlayerId -> PartialGameState -> IO (Int, Maybe Suit)
                       , mkPlay :: PlayerId -> PartialGameState -> IO Card
                       , postBidHook :: PlayerId -> Bid -> PartialGameState -> IO ()
                       , postPlayHook :: PlayerId -> Play -> PartialGameState -> IO ()
                       , acknowledgeTrump :: PlayerId -> Bid -> PartialGameState -> IO ()
                       , name :: String
                       }

instance Show Player where
  show = name
