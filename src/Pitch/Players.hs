{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Pitch.Players (NetworkPlayer (..)
                     ,HumanPlayer (..)
                     ,mkNetworkPlayer
                     ,PartialGameState (..)
                     ,PartialRoundState (..)
                     ,NetStatus (..)
                     )

where

import Pitch.Game
import Pitch.Network
import Pitch.Parser
import Pitch.Card
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.IO.Class
import Data.Aeson (ToJSON, toJSON, FromJSON, fromJSON, encode, decode, Value (..), object, (.:), (.=), parseJSON, (.:?))
import Data.Maybe
import Data.List
import Data.Ord
import Data.Monoid
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data Status = Success String | Failure String

instance ToJSON Status where
  toJSON (Success m) = object ["code" .= toJSON (20 :: Int), "message" .= toJSON m]
  toJSON (Failure m) = object ["code" .= toJSON (50 :: Int), "message" .= toJSON m]


data NetworkPlayer = NetworkPlayer {readChannel :: Chan String
                                   ,writeChannel :: Chan (Status, GameState, [Card])
                                   ,state :: MVar (Writer [String] (GameState, [Card]))
                                   ,thread :: ThreadId
                                   ,name :: String
                                   }
data PartialRoundState = PartialRound [Bid] Suit [Trick] deriving (Show)
data PartialGameState = PartialGame [Int] [String] [PartialRoundState]  deriving (Show)                   

data NetStatus = NetStatus [String] (PartialGameState, [Card]) deriving (Show)

instance FromJSON (PartialGameState, [Card]) where
  parseJSON (Object v) = (,) <$>
                       v .: "gamestate" <*>
                       v .: "hand"
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

instance ToJSON (GameState, [Card]) where
  toJSON (gs, hand) = object ["gamestate" .= toJSON gs
                             ,"hand" .= toJSON hand
                             ]

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
  
instance ToJSON GameState where  
  toJSON Game{players=ps, scores=ss, rounds=rounds} = object ["scores" .= toJSON ss
                                                             ,"players" .= toJSON ps
                                                             ,"rounds" .= toJSON rounds
                                                             ]
instance FromJSON PartialGameState where
  parseJSON (Object v) = PartialGame <$>
                          v .: "scores" <*>
                          v .: "players" <*>
                          v .: "rounds"
  parseJSON _ = mzero
  
instance ToJSON Player where                                                      
  toJSON p = String . T.pack $ show p
  
instance ToJSON RoundState where  
  toJSON Round {bids=bids, trump=trump, tricks=tricks} = object ["bids" .= toJSON bids
                                                                ,"trump" .= toJSON trump
                                                                ,"tricks" .= toJSON tricks
                                                                ]
instance FromJSON PartialRoundState where  
  parseJSON (Object v) = PartialRound <$>
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

mkNetworkPlayer :: String -> IO NetworkPlayer
mkNetworkPlayer n = do rchannel <- newChan
                       wchannel <- newChan
                       state <- newEmptyMVar
                       threadId <- forkIO $ runServer (rchannel, wchannel, state)
                       return NetworkPlayer {readChannel = rchannel
                                            ,writeChannel = wchannel
                                            ,state = state
                                            ,thread = threadId
                                            ,name = n
                                            }

instance Show NetworkPlayer where
    show NetworkPlayer {name = n} = n

buildBid :: Int -> Maybe String -> Maybe (Int, Maybe Suit)
buildBid i (Just suitString) = do s <- parseSuit suitString
                                  return (i, Just s)
buildBid i Nothing = Just (i, Nothing)
                           

instance FromJSON (Int, Maybe Suit) where
    parseJSON (Object v) = do b <- buildBid <$>
                                    v .: "amount" <*>
                                    v .:? "suit"
                              case b of
                                Nothing -> mzero
                                Just x -> return x                              
     -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero
    

instance PlayerLogic NetworkPlayer where
    mkBid p@(NetworkPlayer {readChannel = rchannel, writeChannel = wchannel}, idx) gs hand = 
        do string <- readChan rchannel
           let maybeBid = decode (pack string) :: Maybe (Int, Maybe Suit)
           bid <- case maybeBid of
                    Just x -> return x
                    Nothing -> do writeChan wchannel (Failure "Parser error", gs, hand)
                                  mkBid p gs hand
           validatedBid <- case validateBid idx gs bid of 
                            Nothing -> return bid
                            Just errorMessage -> do writeChan wchannel (Failure errorMessage, gs, hand)
                                                    mkBid p gs hand
           writeChan wchannel (Success "Bid accepted", gs, hand)
           return validatedBid                   

    mkPlay p@(NetworkPlayer {readChannel = channel}, idx) gs hand =
       do string <- readChan channel
          let maybeCard = decode (pack string) :: Maybe Card
          case maybeCard of 
            Just x -> return x
            Nothing -> mkPlay p gs hand

    -- postBidHook :: (p, Int) -> Bid -> GameState -> IO ()
    postBidHook (NetworkPlayer {state= state}, idx) Bid {amount=amount,bidSuit=suit,bidderIdx=bidderIdx} gs hand = 
      do mw <- tryTakeMVar state
         case mw of 
          Nothing -> putMVar state (writer ((gs, hand), [bidMessage]))
          Just w -> do let newW = do w
                                     if (idx /= bidderIdx) then tell [bidMessage]
                                                           else return ()
                                     return (gs, hand)
                       putMVar state newW
         return ()
      where bidMessage = case amount of
                           0 -> show (players gs !! bidderIdx) ++ " passed"
                           x -> show (players gs !! bidderIdx) ++ " bid " ++ show x



data HumanPlayer = Human String deriving (Eq)

instance Show HumanPlayer where
    show (Human name) = name

prompt :: (Read a, MonadIO m) => String -> m a
prompt s = liftIO $ do putStrLn s
                       string <- getLine
                       case reads string of
                         [(x, "")] -> return x
                         _ -> do putStrLn $ "Couldn't parse input " ++ show string
                                 prompt s

promptp :: (MonadIO m) => (String -> Maybe a) -> String -> m a
promptp parseFun s = liftIO $ do putStrLn s
                                 string <- getLine
                                 case parseFun string of
                                   Just x -> return x
                                   Nothing -> do putStrLn $ "Couldn't parse input " ++ show string
                                                 promptp parseFun s

validatePrompt :: (MonadIO m) => String -> (String -> Maybe a) -> (a -> Maybe String) -> m a
validatePrompt promptMessage parser validator =
  do val <- promptp parser promptMessage
     case validator val  of
         Nothing -> return val
         Just errorMessage -> do liftIO $ putStrLn errorMessage
                                 validatePrompt promptMessage parser validator

instance PlayerLogic HumanPlayer where
    mkBid (p, idx) gs@Game{rounds=round:rs} hand = 
        do liftIO $ putStrLn (show p ++ ", it is your turn to bid")
           if all (==0) (map amount $ bids round)
              then liftIO $ putStrLn "Nobody has bid yet"
              else let maxBid = maximumBy (comparing amount) $ bids round
                       bidder = players gs !! bidderIdx maxBid
                   in liftIO $ putStrLn (show bidder ++ " has bid " ++ show (amount maxBid))
           liftIO $ putStr "Your hand is "
           liftIO $ print hand
           bid <- validatePrompt "What is your bid?" 
                                 parseInt $
                                 validateBidAmount idx gs
           suit <- if bid /= 0
                   then do s <- validatePrompt "What suit?" 
                                               parseSuit $
                                               validateBidSuit idx gs bid . Just
                           return (Just s)
                   else return Nothing
           return (bid, suit)

    mkPlay (p, idx) gs@Game{rounds=rounds@Round{trump=trump
                                               ,tricks=trick@Trick{played=played}:ts
                                               }:rs
                            ,players=players
                            } 
                    hand =
        do liftIO $ putStrLn (show p ++ ", it is your turn to play")
           if null played
               then liftIO $ putStrLn "Your lead"
               else let trickPlayers = map ((players !!) . playerIdx) played
                        first:rest = reverse $ zip played trickPlayers
                    in do liftIO $ putStrLn (show (snd first) ++ " led with " ++ show (card $ fst first))
                          forM_ rest (\(play@Play{card=card}, player) -> liftIO $ putStrLn (show player ++ " followed with " ++ show card))
           liftIO $ putStr "Your hand is "
           liftIO $ print hand
           validatePrompt "What card you do you want to play?"
                          parseCard $
                          validateCard idx gs
                     

