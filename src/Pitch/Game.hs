{-# LANGUAGE ExistentialQuantification #-}

module Pitch.Game (mkRoundState
                  ,mkGameState
                  ,playGame
                  ,Player (..)
                  ,HumanPlayer (..)
                  ,PlayerLogic (..)
                  ,prompt
                  ,GameState (players, scores, rounds)
                  ,RoundState (bids, trump, tricks)
                  )

where

import           Control.Monad
import           Control.Monad.State
import           Data.List
import           Data.Ord
import           Pitch.Card
import           Pitch.Deck
import           Pitch.Parser
import           Pitch.Utils
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
doBid (Player p, idx) = do g <- get
                           (amount, suit) <- liftIO $ mkBid (p, idx) g
                           g@Game {rounds=r@Round{bids=bs}:rs} <- get
                           let newBid = Bid amount suit idx
                           put g{rounds=r{bids=newBid:bs}:rs}
                           return ()

doPlay :: (RandomGen g) => (Player, Int) -> Game g ()
doPlay (Player p, idx) = do g <- get
                            card <- liftIO $ mkPlay (p, idx) g
                            g@Game {rounds=r@Round{tricks=t@Trick{played=played}:ts,hands=hands}:rs} <- get
                            let newPlay = Play card idx
                            put g{rounds=r{tricks=t{played=newPlay:played}:ts
                                          ,hands=removeCard card hands
                                          }:rs}
                            return ()
  where removeCard c [] = []
        removeCard c (h@Hand{cards=cs,ownerIdx=i}:hs) | i == idx = h{cards=delete c cs}:hs
                                                      | otherwise = h:removeCard c hs

trickWinner :: Suit -> Trick -> Trick
trickWinner trump trick@(Trick plays winner) 
    | winner /= -1 = trick
    | otherwise = let trumpPlayed = filter ((== trump) . suit . card) plays
                      firstPlayed = card $ last plays
                      Play _ idx = maximumBy (comparing (rank . card))
                                   $ if not $ null trumpPlayed
                                      then trumpPlayed
                                      else filter ((== suit firstPlayed) . suit . card) plays
                   in Trick plays idx

tallyScore :: Suit -> [Trick] -> (Player, Int) -> (Int, Int)
tallyScore trump tricks (p, idx) = 
    let tricksWon = filter ((== idx) . winnerIdx) tricks
        cardsWon = map card $ concatMap played tricksWon
        trumpWon = filter ((== trump) . suit) cardsWon
        allCards = map card $ concatMap played tricks
        allTrump = filter ((== trump) . suit) allCards
        jack = if Card Jack trump `elem` cardsWon
               then 1
               else 0
        hi = if not (null trumpWon) && maximumBy (comparing rank) trumpWon
                                    == maximumBy (comparing rank) allTrump
             then 1 
             else 0
        lo = if not (null trumpWon) && minimumBy (comparing rank) trumpWon
                                    == minimumBy (comparing rank) allTrump
             then 1
             else 0 
        game = sum $ map gamePoints cardsWon
    in (hi + lo + jack, game)

gamePoints :: Card -> Int
gamePoints (Card r _) = rankToGame r
    where rankToGame Jack = 1
          rankToGame Queen = 2
          rankToGame King = 3
          rankToGame Ace = 4
          rankToGame (Number 10) = 10
          rankToGame _ = 0

playTrick :: RandomGen g => Game g Trick
playTrick = do g@Game {rounds=r@Round{bids=bids, tricks=ts}:rs, dealers=ds, players=ps} <- get
               let startingPlayerIdx = if null ts
                                       then bidderIdx $ maximumBy (comparing amount) bids
                                       else winnerIdx $ head ts
               put g{rounds=r{tricks=newTrick:ts}:rs}
               let playOrder = take (length ps) $ dropWhile ((/= startingPlayerIdx) . snd) ds
               forM_ playOrder doPlay
               g@Game {rounds=r@Round{tricks=t:ts, trump=trump}:rs} <- get
               liftIO $ print trump
               liftIO $ print t
               let t' = trickWinner trump t
               put g{rounds=r{tricks=t':ts}:rs}
               return t'

playRound :: RandomGen g => Game g ()
playRound = do  liftIO $ putStrLn "playing a round"
                g@Game {players=ps
                       ,generator=gtr
                       ,rounds=rs
                       ,dealers=ds
                       ,scores=ss
                       } <- get
                let (r, gtr') = mkRoundState gtr (length ps)
                put g{generator=gtr', rounds=r:rs}
                forM_ (take (length ps) ds) doBid
                g@Game {dealers=d:ds, rounds=r@Round{bids=bids}:rs} <- get
                let maxBid = maximumBy (comparing amount) bids
                put g{dealers=ds, rounds=r{trump=bidSuit maxBid}:rs}
                let bidder = ps !! bidderIdx maxBid
                liftIO $ printf "Trump is %s, %s starts\n" (show $ bidSuit maxBid) (show bidder)
                tricks <- forM [1 .. 6] (const playTrick)
                let playersWithIndex = zip ps [0..]
                liftIO $ print tricks
                let roundTalliesAndGame = map (tallyScore (bidSuit maxBid) tricks) playersWithIndex
                let roundTallies = map (\(pts, game) -> if game == maximum (map snd roundTalliesAndGame)
                                                        then pts + 1
                                                        else pts) roundTalliesAndGame
                let roundScores = mapWithIndex (\(idx, score) -> if idx /= bidderIdx maxBid
                                                                    || roundTallies !! idx >= amount maxBid
                                                                 then score
                                                                 else (-1 * amount maxBid))
                                               roundTallies
                let newScores = zipWith (+) roundScores ss
                liftIO . putStr $ "Scores: " ++ show newScores
                put g{scores=newScores}                              
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

class (Show p) => PlayerLogic p where
    --  The result of the state is the bid and the suit that the player will choose
    -- if they win the bid
    mkBid :: (RandomGen g) => (p, Int) -> GameState g -> IO (Int, Suit)

    mkPlay :: (RandomGen g) => (p, Int) -> GameState g -> IO Card

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
    mkBid (p, idx) gs@Game{rounds=round:rs} = 
        do liftIO $ putStrLn (show p ++ ", it is your turn to bid")
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
           bid <- validatePrompt "What is your bid?" 
                                 [(flip elem [0, 2, 3, 4], "A valid bid is 0 (pass), 2, 3, or 4")
                                 ,(\x -> x == 0 || all (<x) (map amount $ bids round)
                                  , "You need to bid more than " ++ 
                                    show (maximum . map amount $ bids round)
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

    mkPlay (p, idx) gs@Game{rounds=rounds@Round{trump=trump
                                               ,tricks=trick@Trick{played=played}:ts
                                               ,hands=hands
                                               }:rs
                            ,players=players
                            } =
        do liftIO $ putStrLn (show p ++ ", it is your turn to play")
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


data Player = forall p. (PlayerLogic p) => Player {getLogic :: p}

instance Show Player where
    show (Player p) = show p



