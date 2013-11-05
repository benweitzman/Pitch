{-# LANGUAGE ExistentialQuantification #-}

module Pitch.Game (RoundState
                  ,mkRoundState
                  ,mkGameState
                  ,playGame
                  ,Player (..)
                  ,HumanPlayer (..)
                  ,prompt
                  )

where

import           Control.Monad
import           Control.Monad.State
import           Data.List
import           Data.Ord
import           Pitch.Card
import           Pitch.Deck
import           Pitch.Parser
import           System.Random
import           Text.Printf         (printf)


data Hand = Hand {cards :: [Card], ownerIdx :: Int} deriving (Show, Eq)
data Bid = Bid {amount :: Int, bidSuit :: Suit, bidderIdx :: Int} deriving (Show, Eq)
data Play = Play {card :: Card, playerIdx :: Int} deriving (Show, Eq)
data Trick = Trick {played :: [Play], winnerIdx :: Int} deriving (Show, Eq)

newTrick :: Trick
newTrick = Trick [] (-1)

data RoundState = Round { deck   :: Deck
                        , hands  :: [Hand]
                        , bids   :: [Bid]
                        , trump  :: Suit
                        , tricks :: [Trick]
                        } deriving (Show)

mkRoundState :: RandomGen g => g -> Int -> (RoundState, g)
mkRoundState g ps = (Round {deck = deck'
                           ,hands = zipWith Hand hs [0..]
                           ,bids = []
                           ,trump = minBound
                           ,tricks = []
                           }
                    ,g'
                    )
    where (hs, deck') = runState (forM [0..ps-1] (const $ deal 6)) d
          (d, g') = mkDeck g

wonBid :: Int -> RoundState -> Bool
wonBid n rs = bidderIdx (maximumBy (comparing amount) (bids rs)) == n

data (RandomGen g) => GameState g = Game { players   :: [Player]
                                         , scores    :: [Int]
                                         , rounds    :: [RoundState]
                                         , dealers   :: [(Player, Int)]
                                         , generator :: g
                                         } deriving (Show)

mkGameState :: RandomGen g => g -> [Player] -> GameState g
mkGameState g ps = Game {players = ps
                                     ,scores = map (const 0) ps
                                     ,generator = g
                                     ,rounds = []
                                     ,dealers = cycle $ zip ps [0..]
                                     }

lastRound :: RandomGen g => GameState g -> RoundState
lastRound = head . rounds

type Game g =  StateT (GameState g) IO

checkForWinner :: RandomGen g => Game g (Maybe (Player, Int))
checkForWinner = state (\x -> (check x, x))
    where check gs = if elem 11 $ scores gs
                     then let scoresAndIndexes = filter (\x -> fst x == 11) $ zip (scores gs) [1..]
                          in do (winnerIdx, score) <- find (\(widx, _) -> wonBid widx (lastRound gs)) scoresAndIndexes
                                return (players gs !! winnerIdx, score)
                     else Nothing

doBid :: (RandomGen g) => (Player, Int) -> Game g ()
doBid (Player p, idx) = do (amount, suit) <- mkBid (p, idx)
                           g@Game {rounds=r@Round{bids=bs}:rs} <- get
                           let newBid = Bid amount suit idx
                           put g{rounds=r{bids=newBid:bs}:rs}
                           return ()

doPlay :: (RandomGen g) => (Player, Int) -> Game g ()
doPlay (Player p, idx) = do card <- mkPlay (p, idx)
                            g@Game {rounds=r@Round{tricks=t@Trick{played=played}:ts,hands=hands}:rs} <- get
                            let newPlay = Play card idx
                            put g{rounds=r{tricks=t{played=newPlay:played}:ts
                                          ,hands=removeCard card hands
                                          }:rs}
                            return ()
  where removeCard c [] = []
        removeCard c (h@Hand{cards=cs,ownerIdx=i}:hs) | i == idx = h{cards=delete c cs}:hs
                                                      | otherwise = h:removeCard c hs
    
winner :: Suit -> [Play] -> Play
winner _ _ = Play (Card Jack Clubs) 3

playTrick :: RandomGen g => Game g ()
playTrick = do g@Game {rounds=r@Round{bids=bids, tricks=ts}:rs, dealers=ds, players=ps} <- get
               let startingPlayerIdx = if null $ tricks r
                                       then bidderIdx $ maximumBy (comparing amount) bids
                                       else winnerIdx $ head ts
               put g{rounds=r{tricks=newTrick:ts}:rs}
               let playOrder = take 4 $ dropWhile ((/= startingPlayerIdx) . snd) ds
               forM_ playOrder doPlay                              
               return ()

playRound :: RandomGen g => Game g ()
playRound = do  liftIO $ putStrLn "playing a round"
                g@Game {players=ps
                       ,generator=gtr
                       ,rounds=rs
                       ,dealers=ds
                       } <- get
                let (r, gtr') = mkRoundState gtr (length ps)
                put g{generator=gtr', rounds=r:rs}
                forM_ (take (length ps) ds) doBid
                g@Game {dealers=d:ds, rounds=r@Round{bids=bids}:rs} <- get
                let maxBid = maximumBy (comparing amount) bids
                put g{dealers=ds, rounds=r{trump=bidSuit maxBid}:rs}
                let bidder = ps !! bidderIdx maxBid
                liftIO $ printf "Trump is %s, %s starts\n" (show $ bidSuit maxBid) (show bidder)
                forM_ [1 .. 6] (const playTrick)
                return ()

playGame :: (RandomGen g) => Game g (Player, Int)
playGame = do playRound
              maybeWinner <- checkForWinner
              let winnerSt = case maybeWinner of
                                Nothing -> playGame
                                Just x -> return x
              (winP, score) <- winnerSt
              liftIO $ putStrLn "we have a winner"
              liftIO $ print score
              winnerSt

class (Show p, Eq p) => PlayerLogic p where
    --  The result of the state is the bid and the suit that the player will choose
    -- if they win the bid
    mkBid :: (RandomGen g) => (p, Int) -> Game g (Int, Suit)

    mkPlay :: (RandomGen g) => (p, Int) -> Game g Card

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




instance (Show g) => Show (RandomGen g)

instance PlayerLogic HumanPlayer where
    mkBid (p, idx) = do gs <- get
                        let round = head $ rounds gs
                        liftIO $ putStrLn (show p ++ ", it is your turn to bid")
                        if all (==0) (map amount $ bids round)
                           then liftIO $ putStrLn "Nobody has bid yet"
                           else let maxBid = maximumBy (comparing amount) $ bids round
                                    bidder = players gs !! bidderIdx maxBid
                                in liftIO $ putStrLn (show bidder ++ " has bid " ++ show (amount maxBid))
                        liftIO $ putStr "Your hand is "
                        let hand = case find ((== idx) . ownerIdx) (hands round) of
                                     Just h -> cards h
                                     Nothing -> error "This can't happen"
                        liftIO $ print hand
                        bid <- validatePrompt "What is your bid?" [(flip elem [0, 2, 3, 4], "A valid bid is 0 (pass), 2, 3, or 4")
                                                                  ,(\x -> x == 0 || all (<x) (map amount $ bids round)
                                                                   , "You need to bid more than " ++ show (maximum . map amount $ bids round)
                                                                   )
                                                                  ,(\x -> idx /= snd (dealers gs !! (length (players gs) - 1))
                                                                          || (maximum . map amount $ bids round) /= 0
                                                                          || x /= 0
                                                                   , "Since you are the last player to bid and everyone else has passed, you must bid at least 2"
                                                                   )
                                                                  ]
                        suit <- if bid /= 0
                                then prompt "What suit?"
                                else return minBound
                        return (bid, suit)

    mkPlay (p, idx) = do gs@Game{rounds=rounds@Round{trump=trump
                                                    ,tricks=trick@Trick{played=played}:ts
                                                    ,hands=hands                                                            
                                                    }:rs
                                ,players=players
                                } <- get
                         liftIO $ putStrLn (show p ++ ", it is your turn to play")
                         if null played
                            then liftIO $ putStrLn "Your lead"
                            else let trickPlayers = map ((players !!) . playerIdx) played
                                     first:rest = reverse $ zip played trickPlayers
                                 in do liftIO $ putStrLn (show (snd first) ++ " led with " ++ show (card $ fst first))
                                       forM_ rest (\(play@Play{card=card}, player) -> liftIO $ putStrLn (show player ++ " followed with " ++ show card))
                         liftIO $ putStr "Your hand is "
                         let hand = case find ((== idx) . ownerIdx) hands of
                                      Just h -> cards h
                                      Nothing -> error "This can't happen"
                         liftIO $ print hand
                         validatePromptp parseCard "What card you do you want to play?" [(flip elem hand, "You don't have that card")
                                                                                        ,(\c -> not (null ts) || not (null played) || suit c == trump
                                                                                         , "You must lead trump"
                                                                                         )  
                                                                                        ,(\c -> suit c == trump
                                                                                                || suit c == suit (card (last played))
                                                                                                || notElem (suit (card (last played))) (map suit hand)
                                                                                         , "You must play trump or follow suit"
                                                                                         )
                                                                                        ]                      


data Player = forall p. (PlayerLogic p) => Player {getLogic :: p}

instance Show Player where
    show (Player p) = show p



