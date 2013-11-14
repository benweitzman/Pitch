{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Pitch.Players (NetworkPlayer (..)
                     ,HumanPlayer (..)
                     ,mkNetworkPlayer
                     ,PartialGameState (..)
                     ,PartialRoundState (..)
                     ,NetStatus (..)
                     ,ActionRequired (..)
                     ,Status (..)
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
import System.IO
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data Status = Success String | Failure String

instance ToJSON Status where
  toJSON (Success m) = object ["code" .= toJSON (20 :: Int), "message" .= toJSON m]
  toJSON (Failure m) = object ["code" .= toJSON (50 :: Int), "message" .= toJSON m]

mkStatus :: Int -> String -> Status
mkStatus 20 = Success
mkStatus 50 = Failure

instance FromJSON Status where
  parseJSON (Object v) = mkStatus <$>
                           v .: "code" <*>
                           v .: "message"
  parseJSON _ = mzero                           

data NetworkPlayer = NetworkPlayer {readChannel :: Chan String
                                   ,writeChannel :: Chan (Status, PartialGameState, [Card])
                                   ,state :: MVar (Writer [String] (PartialGameState, [Card], ActionRequired, Int))
                                   ,thread :: ThreadId
                                   ,name :: String
                                   }          

data ActionRequired = Wait | BidAction | PlayAction deriving (Show)

data NetStatus = NetStatus {messages :: [String], gamestate :: (PartialGameState, [Card], ActionRequired, Int)} deriving (Show)

instance FromJSON (PartialGameState, [Card], ActionRequired, Int) where
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
  

instance ToJSON (Status, PartialGameState, [Card]) where
  toJSON (status, gs, hand) = object ["status" .= toJSON status
                                     ,"gamestate" .= toJSON gs
                                     ,"hand" .= toJSON hand
                                     ]
                              
instance FromJSON (Status, PartialGameState, [Card]) where                              
  parseJSON (Object v) = (,,) <$>
                           v .: "status" <*>
                           v .: "gamestate" <*>
                           v .: "hand"
  parseJSON _ = mzero
                           

instance ToJSON (PartialGameState, [Card], ActionRequired, Int) where
  toJSON (gs, hand, action, idx) = object ["global" .= toJSON gs
                                          ,"hand" .= toJSON hand
                                          ,"action" .= toJSON action
                                          ,"playerIndex" .= toJSON idx
                                          ]
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
  
instance ToJSON PartialGameState where  
  toJSON (PartialGame ss ps rounds) = object ["scores" .= toJSON ss
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
  
instance ToJSON PartialRoundState where  
  toJSON (PartialRound bids trump tricks) = object ["bids" .= toJSON bids
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
    
setAction :: MVar (Writer [String] (PartialGameState, [Card], ActionRequired, Int)) -> ActionRequired -> IO ()
setAction mvar ar = modifyMVar_ mvar $ \w -> return (do (a, b, _, c) <- w
                                                        return (a, b, ar, c))

setGameState :: MVar (Writer [String] (PartialGameState, [Card], ActionRequired, Int)) -> PartialGameState -> IO ()
setGameState mvar gs = modifyMVar_ mvar $ \w -> return (do (_, a, b, c) <- w
                                                           return (gs, a, b, c))

setHand :: MVar (Writer [String] (PartialGameState, [Card], ActionRequired, Int)) -> [Card] -> IO ()
setHand mvar hand = modifyMVar_ mvar $ \w -> return (do (a, _, b, c) <- w
                                                        return (a, hand, b, c))

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

instance PlayerLogic NetworkPlayer where
    initGameState (p@NetworkPlayer{state=state}, idx) gs hand = updateMVAR state (gs, hand, Wait, idx)

    mkBid p@(NetworkPlayer {readChannel = rchannel, writeChannel = wchannel, state=state}, idx) gs hand = 
        do setAction state BidAction
           setGameState state gs
           string <- readChan rchannel
           let maybeBid = decode (pack string) :: Maybe (Int, Maybe Suit)
           bid <- case maybeBid of
                    Just x -> return x
                    Nothing -> do writeChan wchannel (Failure "Parser error", gs, hand)
                                  mkBid p gs hand
           validatedBid <- case validateBid idx (gs, hand) bid of 
                            Nothing -> return bid
                            Just errorMessage -> do writeChan wchannel (Failure errorMessage, gs, hand)
                                                    mkBid p gs hand
           writeChan wchannel (Success "Bid accepted", gs, hand)
           setAction state Wait
           return validatedBid                   

    mkPlay p@(NetworkPlayer {readChannel = channel, writeChannel = wchannel, state=state}, idx) gs hand =
       do setAction state PlayAction
          setGameState state gs
          string <- readChan channel
          let maybeCard = decode (pack string) :: Maybe Card
          card <- case maybeCard of 
                    Just x -> return x
                    Nothing -> do writeChan wchannel (Failure "Parser error", gs, hand)
                                  mkPlay p gs hand
          validatedCard <- case validateCard idx (gs, hand) card of
                             Nothing -> return card
                             Just errorMessage -> do writeChan wchannel (Failure errorMessage, gs, hand)
                                                     mkPlay p gs hand
          writeChan wchannel (Success "Card accepted", gs, hand)
          setAction state Wait
          return validatedCard                                             

    -- postBidHook :: (p, Int) -> Bid -> GameState -> IO ()
    postBidHook (NetworkPlayer {state= state}, idx) 
                Bid {amount=amount,bidSuit=suit,bidderIdx=bidderIdx} 
                gs@(PartialGame scores players rounds) 
                hand = 
      do putMessage state bidMessage
         setHand state hand
         setGameState state gs
      where bidMessage = case amount of
                           0 -> players !! bidderIdx ++ " passed"
                           x -> players !! bidderIdx ++ " bid " ++ show x

    --  postPlayHook :: (p, Int) -> Play -> PartialGameState -> [Card] -> IO ()                       
    postPlayHook (NetworkPlayer {state=state}, idx)
                 Play {card=card,playerIdx=playerIdx}
                 gs@(PartialGame scores players rounds)
                 hand = 
      do putMessage state playMessage
         setHand state hand
         setGameState state gs
      where playMessage = players !! playerIdx ++ " played " ++ show card

    acknowledgeTrump (NetworkPlayer {state=state}, idx)
                     Bid {amount=amount,bidSuit=Just suit,bidderIdx=bidderIdx} 
                     pgs
                     hand =
      putMessage state $ "Trump is " ++ show suit

data HumanPlayer = Human String deriving (Eq)

instance Show HumanPlayer where
    show (Human name) = name

prompt :: (Read a, MonadIO m) => String -> m a
prompt s = liftIO $ do putStrLn s
                       putStr "> "
                       string <- getLine
                       case reads string of
                         [(x, "")] -> return x
                         _ -> do putStrLn $ "Couldn't parse input " ++ show string
                                 prompt s

promptp :: (MonadIO m) => (String -> Maybe a) -> String -> m a
promptp parseFun s = liftIO $ do putStrLn s
                                 putStr "> "
                                 hFlush stdout
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
    mkBid (p, idx) gs@(PartialGame scores players (PartialRound bids trump tricks:rs)) hand = 
        do liftIO $ putStrLn (show p ++ ", it is your turn to bid")
           if all (==0) (map amount bids)
              then liftIO $ putStrLn "Nobody has bid yet"
              else let maxBid = maximumBy (comparing amount) bids
                       bidder = players !! bidderIdx maxBid
                   in liftIO $ putStrLn (bidder ++ " has bid " ++ show (amount maxBid))
           liftIO $ putStr "Your hand is "
           liftIO $ print hand
           bid <- validatePrompt "What is your bid?" 
                                 parseInt $
                                 validateBidAmount idx (gs, hand)
           suit <- if bid /= 0
                   then do s <- validatePrompt "What suit?" 
                                               parseSuit $
                                               validateBidSuit idx (gs, hand) bid . Just
                           return (Just s)
                   else return Nothing
           return (bid, suit)

    mkPlay (p, idx) gs@(PartialGame scores players (PartialRound bids trump
                                                               (Trick{played=played}:ts
                                               ):rs)) hand = 
        do liftIO $ putStrLn (show p ++ ", it is your turn to play")
           if null played
               then liftIO $ putStrLn "Your lead"
               else let trickPlayers = map ((players !!) . playerIdx) played
                        first:rest = reverse $ zip played trickPlayers
                    in do liftIO $ putStrLn $ snd first ++ " led with " ++ show (card $ fst first)
                          forM_ rest (\(play@Play{card=card}, player) -> liftIO $ putStrLn (show player ++ " followed with " ++ show card))
           liftIO $ putStr "Your hand is "
           liftIO $ print hand
           validatePrompt "What card you do you want to play?"
                          parseCard $
                          validateCard idx (gs, hand)
                     

