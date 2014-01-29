module Pitch.Internal.Game (GameState (..)
                           ,RoundState(..)
                           ,P.Player(..)
                           )
where

import           Pitch.Card
import           Pitch.Deck
import           System.Random
import Control.Monad.State
import Pitch.Internal.Common
import qualified Pitch.Internal.PartialGame as P

data GameState = Game { players   :: [(PlayerId, P.Player)]
                      , scores    :: [(PlayerId, Int)]
                      , rounds    :: [RoundState]
                      , dealers   :: [(PlayerId, P.Player)]
                      , generator :: StdGen
                      } 
                 
instance Show GameState where                  
  show Game{players=p
           ,scores=s
           ,rounds=r
           ,dealers=d
           ,generator=g
           } = 
    "Game { players = " ++ show p ++ ", scores = " ++ show s ++ ", rounds = " ++ show r ++ ", dealers = " ++ show (take (length p) d) ++ "..., generator = " ++ show g ++ " }"
                 

data RoundState = Round { deck   :: Deck
                        , hands  :: [Hand]
                        , bids   :: [Bid]
                        , trump  :: Suit
                        , tricks :: [Trick]
                        } deriving (Show)



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
