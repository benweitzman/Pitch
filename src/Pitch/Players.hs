{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Pitch.Players (NetworkPlayer (..)
                     ,HumanPlayer (..)
                     ,mkNetworkPlayer
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
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.List
import Data.Ord
import Data.Monoid
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text as T

data NetworkPlayer = NetworkPlayer {readChannel :: Chan String
                                   ,writeChannel :: Chan (GameState, [Card])
                                   ,thread :: ThreadId
                                   ,name :: String
                                   }

instance ToJSON Card where
  toJSON (Card rank suit) = object ["rank" .= toJSON rank, "suit" .= toJSON suit]
  
instance ToJSON Suit where  
  toJSON suit = String . T.pack $ show suit
  
instance ToJSON Rank where
  toJSON rank = String . T.pack $ show rank
  
instance ToJSON GameState where  
  toJSON Game{players=ps, scores=ss, rounds=rounds} = object ["scores" .= toJSON ss
                                                             ,"players" .= toJSON ps
                                                             ,"rounds" .= toJSON rounds
                                                             ]
instance ToJSON Player where                                                      
  toJSON p = String . T.pack $ show p
  
instance ToJSON RoundState where  
  toJSON Round {bids=bids, trump=trump, tricks=tricks} = object ["bids" .= toJSON bids
                                                                ,"trump" .= toJSON trump
                                                                ,"tricks" .= toJSON tricks
                                                                ]
instance ToJSON Bid where
  toJSON Bid{amount=a,bidSuit=s,bidderIdx=idx} = object ["amount" .= toJSON a
                                                        ,"suit" .= toJSON s
                                                        ,"bidderIndex" .= toJSON idx
                                                        ]                                                 
instance ToJSON Trick where
  toJSON Trick{played=p, winnerIdx=idx} = object ["played" .= toJSON p
                                                 ,"winnerIndex" .= toJSON idx
                                                 ]                                          
instance ToJSON Play where
  toJSON Play{card=c, playerIdx=idx} = object ["playerIndex" .= idx
                                              ,"cad" .= toJSON c
                                              ]                                              

mkNetworkPlayer :: String -> IO NetworkPlayer
mkNetworkPlayer n = do rchannel <- newChan
                       wchannel <- newChan
                       threadId <- forkIO $ runServer (rchannel, wchannel)
                       return NetworkPlayer {readChannel = rchannel
                                            ,writeChannel = wchannel
                                            ,thread = threadId
                                            ,name = n
                                            }

instance Show NetworkPlayer where
    show NetworkPlayer {name = n} = n

buildBid :: Int -> String -> Maybe (Int, Suit)
buildBid i suitString = do s <- parseSuit suitString
                           return (i, s)
                           
buildPlay :: String -> Maybe Card                           
buildPlay = parseCard

instance FromJSON (Int, Suit) where
    parseJSON (Object v) = do b <- buildBid <$>
                                    v .: "amount" <*>
                                    v .: "suit"
                              case b of
                                Nothing -> mzero
                                Just x -> return x                              
     -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero
    
instance FromJSON Card where
  parseJSON (Object v) = do c <- buildPlay <$>
                                  v .: "card"
                            case c of      
                              Nothing -> mzero
                              Just x -> return x

instance PlayerLogic NetworkPlayer where
    mkBid p@(NetworkPlayer {readChannel = channel}, idx) gs hand = 
        do string <- readChan channel
           let maybeBid = decode (pack string) :: Maybe (Int, Suit)
           case maybeBid of
             Just x -> return x
             Nothing -> mkBid p gs hand

    mkPlay p@(NetworkPlayer {readChannel = channel}, idx) gs hand =
       do string <- readChan channel
          let maybeCard = decode (pack string) :: Maybe Card
          case maybeCard of 
            Just x -> return x
            Nothing -> mkPlay p gs hand



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

validateOne :: (Read a, MonadIO m) => String -> (a -> Bool) -> String -> m a
validateOne promptMessage predicate errorMessage =
    do val <- prompt promptMessage
       if predicate val
           then return val
           else do liftIO $ putStrLn errorMessage
                   validateOne promptMessage predicate errorMessage

validatePrompt :: (Read a, MonadIO m) => String -> [(a -> Bool, String)] -> m a
validatePrompt promptMessage tests =
    do val <- prompt promptMessage
       case find (\(p,_) -> not $ p val) tests of
         Nothing -> return val
         Just (_, errorMessage) -> do liftIO $ putStrLn errorMessage
                                      validatePrompt promptMessage tests

validatePromptp :: (MonadIO m) => (String -> Maybe a) -> String -> [(a -> Bool, String)] -> m a
validatePromptp parseFun promptMessage tests =
  do val <- promptp parseFun promptMessage
     case find (\(p,_) -> not $ p val) tests of
         Nothing -> return val
         Just (_, errorMessage) -> do liftIO $ putStrLn errorMessage
                                      validatePromptp parseFun promptMessage tests

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
                                 [(flip elem [0, 2, 3, 4], "A valid bid is 0 (pass), 2, 3, or 4")
                                 ,(\x -> x == 0 || all (<x) (map amount $ bids round)
                                  , "You need to bid more than " ++ 
                                    show (maximum . map amount $ bids round)
                                  )
                                 ,(\x -> length (bids round) /= length (players gs) - 1
                                      || (maximum . map amount $ bids round) /= 0
                                      || x /= 0
                                  , "Since you are the last player to bid and everyone else has passed, you must bid at least 2"
                                  )
                                 ]
           suit <- if bid /= 0
                   then prompt "What suit?"
                   else return minBound
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
           validatePromptp parseCard
                           "What card you do you want to play?"
                           [(flip elem hand, "You don't have that card")
                           ,(\c -> not (null ts) || not (null played) || suit c == trump
                           , "You must lead trump"
                           )
                           ,(\c -> suit c == trump
                               || null played
                               || suit c == suit (card (last played))
                               || notElem (suit (card (last played))) (map suit hand)
                           , "You must play trump or follow suit"
                           )
                           ]

