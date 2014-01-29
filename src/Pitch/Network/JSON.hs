{-# LANGUAGE OverloadedStrings #-}

module Pitch.Network.JSON where

import Control.Applicative
import Control.Monad
import Data.Aeson (ToJSON, toJSON, FromJSON, fromJSON, encode, decode, Value (..), object, (.:), (.=), parseJSON, (.:?))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Pitch.Game
import Pitch.Card
import Pitch.Parser

instance FromJSON GameState
instance ToJSON GameState
instance FromJSON RoundState
instance ToJSON RoundState
instance FromJSON Trick
instance ToJSON Trick
instance FromJSON Hand
instance ToJSON Hand
instance FromJSON Bid
instance ToJSON Bid
instance FromJSON Play
instance ToJSON Play


instance ToJSON Card where
  toJSON (Card rank suit) = object ["rank" .= toJSON rank, "suit" .= toJSON suit]
  
buildPlay :: String -> Maybe Card                           
buildPlay = parseCard

instance FromJSON Card where
  parseJSON (Object v) = if "card" `HM.member` v
                         then do c <- buildPlay <$>
                                       v .: "card"
                                 case c of      
                                   Nothing -> mzero
                                   Just x -> return x
                         else Card <$>
                               v .: "rank" <*>
                               v .: "suit"
  parseJSON (String v) = case parseCard (T.unpack v) of 
                           Just c -> return c
                           Nothing -> mzero
  parseJSON _          = mzero  

instance ToJSON Suit where  
  toJSON suit = String . T.pack $ show suit
  
instance FromJSON Suit where
  parseJSON (String v) = case parseSuit (T.unpack v) of 
                           Just s -> return s
                           Nothing -> mzero
  parseJSON _ = mzero

instance ToJSON Rank where
  toJSON rank = String . T.pack $ show rank
  
instance FromJSON Rank where  
  parseJSON (String v) = case parseRank (T.unpack v) of
                           Just r -> return r
                           Nothing -> mzero
  parseJSON _ = mzero

{-

instance FromJSON (GameState, [Card], ActionRequired, Int) where
  parseJSON (Object v) = (,,,) <$>
                       v .: "global" <*>
                       v .: "hand" <*>
                       v .: "action" <*>
                       v .: "playerIndex"
  parseJSON _ = mzero

instance FromJSON NetStatus where
  parseJSON (Object v) = NetStatus <$> 
                           v .: "messages" <*>
                           v .: "gamestate"
  parseJSON _          = mzero
  

instance ToJSON (Status, GameState, [Card]) where
  toJSON (status, gs, hand) = object ["status" .= toJSON status
                                     ,"gamestate" .= toJSON gs
                                     ,"hand" .= toJSON hand
                                     ]
                              
instance FromJSON (Status, GameState, [Card]) where                              
  parseJSON (Object v) = (,,) <$>
                           v .: "status" <*>
                           v .: "gamestate" <*>
                           v .: "hand"
  parseJSON _ = mzero
                           

instance ToJSON (GameState, [Card], ActionRequired, Int) where
  toJSON (gs, hand, action, idx) = object ["global" .= toJSON gs
                                          ,"hand" .= toJSON hand
                                          ,"action" .= toJSON action
                                          ,"playerIndex" .= toJSON idx
                                          ]                           

  
instance ToJSON GameState where  
  toJSON (Game ss ps rounds) = object ["scores" .= toJSON ss
                                      ,"players" .= toJSON ps
                                      ,"rounds" .= toJSON rounds
                                      ]
instance FromJSON GameState where
  parseJSON (Object v) = Game <$>
                          v .: "scores" <*>
                          v .: "players" <*>
                          v .: "rounds"
  parseJSON _ = mzero
  
instance ToJSON Player where                                                      
  toJSON p = String . T.pack $ show p
  
instance ToJSON RoundState where  
  toJSON (Round bids trump tricks) = object ["bids" .= toJSON bids
                                                   ,"trump" .= toJSON trump
                                                   ,"tricks" .= toJSON tricks
                                                   ]
instance FromJSON RoundState where  
  parseJSON (Object v) = Round <$>
                          v .: "bids" <*>
                          v .: "trump" <*>
                          v .: "tricks"
  parseJSON _ = mzero

instance ToJSON Bid where
  toJSON Bid{amount=a,bidSuit=s,bidderIdx=idx} = object ["amount" .= toJSON a
                                                        ,"suit" .= toJSON s
                                                        ,"bidderIndex" .= toJSON idx
                                                        ]                                                 
                                                 
instance FromJSON Bid where
  parseJSON (Object v) = Bid <$>
                         v .: "amount" <*>
                         v .: "suit" <*>
                         v .: "bidderIndex"
  parseJSON _ = mzero                                                  
                                                 
instance ToJSON Trick where
  toJSON Trick{played=p, winnerIdx=idx} = object ["played" .= toJSON p
                                                 ,"winnerIndex" .= toJSON idx
                                                 ]                                          
instance FromJSON Trick where 
  parseJSON (Object v) = Trick <$>
                          v .: "played" <*>
                          v .: "winnerIndex"
  parseJSON _ = mzero                                  

instance ToJSON Play where
  toJSON Play{card=c, playerIdx=idx} = object ["playerIndex" .= idx
                                              ,"card" .= toJSON c
                                              ]                                              
                                       
instance FromJSON Play where
  parseJSON (Object v) = Play <$>
                          v .: "card" <*>
                          v .: "playerIndex"
  parseJSON _ = mzero                          

instance Show NetworkPlayer where
    show NetworkPlayer {name = n} = n

buildBid :: Int -> Maybe String -> Maybe (Int, Maybe Suit)
buildBid i (Just suitString) = do s <- parseSuit suitString
                                  return (i, Just s)
buildBid i Nothing = Just (i, Nothing)
                           
instance ToJSON (Int, Maybe Suit) where
  toJSON (x, Nothing) = object ["amount" .= toJSON x]
  toJSON (x, Just y) = object ["amount" .= toJSON x, "suit" .= toJSON y]

instance FromJSON (Int, Maybe Suit) where
    parseJSON (Object v) = do b <- buildBid <$>
                                    v .: "amount" <*>
                                    v .:? "suit"
                              case b of
                                Nothing -> mzero
                                Just x -> return x                              
     -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero
-}