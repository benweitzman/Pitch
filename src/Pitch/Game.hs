{-# LANGUAGE ExistentialQuantification, RankNTypes, Rank2Types #-}

module Pitch.Game (mkRoundState
                  ,mkGameState
                  ,playGame
                  ,Player (..)
                  ,PlayerLogic (..)
                  ,Hand (..)
                  ,Bid (..)
                  ,Play (..)
                  ,Trick (..)
                  ,validateBidAmount
                  ,validateBidSuit
                  ,validateCard
                  ,validateBid
                  ,PartialGameState (..)
                  ,PartialRoundState (..)
                  )

where

import           Control.Monad
import           Control.Monad.State
import           Data.List
import           Data.Maybe 
import           Data.Monoid
import           Data.Ord
import           Pitch.Card
import           Pitch.Deck
import           Pitch.Parser
import           Pitch.Utils
import           System.Random
import           Text.Printf         (printf)

data Hand = Hand {cards :: [Card], ownerIdx :: Int} deriving (Show, Eq)
data Bid = Bid {amount :: Int, bidSuit :: Maybe Suit, bidderIdx :: Int} deriving (Show, Eq)
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

mkRoundState :: StdGen -> Int -> (RoundState, StdGen)
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

data PartialRoundState = PartialRound [Bid] Suit [Trick] deriving (Show)
data PartialGameState = PartialGame [Int] [String] [PartialRoundState]  deriving (Show)   

partialRound :: RoundState -> PartialRoundState
partialRound Round{bids=bids,trump=trump,tricks=tricks} = 
  PartialRound bids trump tricks

partialGame :: GameState -> PartialGameState
partialGame Game{scores=scores,players=players,rounds=rounds} = 
  PartialGame scores (map show players) (map partialRound rounds)

wonBid :: Int -> RoundState -> Bool
wonBid n rs = bidderIdx (maximumBy (comparing amount) (bids rs)) == n

data GameState = Game { players   :: [Player]
                      , scores    :: [Int]
                      , rounds    :: [RoundState]
                      , dealers   :: [(Player, Int)]
                      , generator :: StdGen
                      }
                                 
mkGameState :: StdGen -> [Player] -> GameState
mkGameState g ps = Game {players = ps
                        ,scores = map (const 0) ps
                        ,generator = g
                        ,rounds = []
                        ,dealers = cycle $ zip ps [0..]
                        }

lastRound :: GameState -> RoundState
lastRound = head . rounds

type Game =  StateT GameState IO

checkForWinner :: Game (Maybe (Player, Int))
checkForWinner = state (\x -> (check x, x))
    where check gs = if elem 11 $ scores gs
                     then let scoresAndIndexes = filter (\x -> fst x == 11) $ zip (scores gs) [1..]
                          in do (winnerIdx, score) <- find (\(widx, _) -> wonBid widx (lastRound gs)) scoresAndIndexes
                                return (players gs !! winnerIdx, score)
                     else Nothing

validateBidAmount :: Int -> (PartialGameState, [Card]) -> Int -> Maybe String
validateBidAmount idx gs@(PartialGame scores ps (r@(PartialRound bids trump tricks):rs)
                         ,hand) x
  | x `notElem` [0, 2, 3, 4] = Just "A valid bid is 0 (pass), 2, 3, or 4"
  | x /= 0 && any (>= x) (map amount bids) = Just $ "You need to bid more than " ++ show (maximum $ map amount bids)
  | x == 0 && length bids + 1 == length ps && maximum (map amount bids) == 0 = Just "Since you are the last player to bid and everyone else has passed, you must bid at least 2"
  | otherwise = Nothing

validateBidSuit :: Int -> (PartialGameState, [Card]) -> Int -> Maybe Suit -> Maybe String
validateBidSuit _ _ x Nothing
  | x /= 0 = Just "You must specify a suit"
  | otherwise = Nothing
validateBidSuit idx (_, hand) x (Just s) 
  | s `notElem` map suit hand = Just "You don't have any cards of that suit"
  | otherwise = Nothing

validateBid :: Int -> (PartialGameState, [Card]) -> (Int, Maybe Suit) -> Maybe String
validateBid idx q (x, s) = getFirst $ First (validateBidAmount idx q x) <> First (validateBidSuit idx q x s)

validateCard :: Int -> (PartialGameState, [Card]) -> Card -> Maybe String
validateCard idx (PartialGame scoresp ps (PartialRound bids trump (Trick{played=played}:ts):rs)
                 , hand) c 
  | c `notElem` hand = Just "You don't have that card"
  | null ts && null played && suit c /= trump = Just "You must lead trump"
  | suit c /= trump && not (null played) 
 && suit c /= suit (card (last played)) 
 && suit (card (last played)) `notElem` map suit hand
  = Just "You must play trump or follow suit"
  | otherwise = Nothing

doBid :: (Player, Int) -> Game ()
doBid (Player p, idx) = do g@Game{players=ps,rounds=r@Round{hands=hands}:rs} <- get
                           let hand = cards . fromJust $ find ((== idx) . ownerIdx) hands
                           (amount, suit) <- liftIO $ mkBid (p, idx) (partialGame g) hand
                           g@Game {rounds=r@Round{bids=bs}:rs} <- get
                           let newBid = Bid amount suit idx
                           put g{rounds=r{bids=newBid:bs}:rs}
                           liftIO $ forM_ (zip ps [0..]) (\(Player p, x) -> postBidHook (p, x) newBid (partialGame g{rounds=r{bids=newBid:bs}:rs}) (cards . fromJust $ find ((== x) .ownerIdx) hands))
                           return ()

doPlay :: (Player, Int) -> Game ()
doPlay (Player p, idx) = do g@Game{players=ps,rounds=r@Round{hands=hands}:rs} <- get
                            let hand = cards . fromJust $ find ((== idx) . ownerIdx) hands
                            card <- liftIO $ mkPlay (p, idx) (partialGame g) hand
                            g@Game {rounds=r@Round{tricks=t@Trick{played=played}:ts,hands=hands}:rs} <- get
                            let newPlay = Play card idx
                            let newGS = g{rounds=r{tricks=t{played=newPlay:played}:ts
                                                  ,hands=removeCard card hands
                                                  }:rs}
                            put newGS
                            liftIO $ forM_ (zip ps [0..]) (\(Player p, x) -> postPlayHook (p, x) newPlay (partialGame newGS) (cards . fromJust $ find ((== x) .ownerIdx) hands))
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

playTrick :: Game Trick
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

playRound :: Game ()
playRound = do  liftIO $ putStrLn "playing a round"
                g@Game {players=ps
                       ,generator=gtr
                       ,rounds=rs
                       ,dealers=ds
                       ,scores=ss
                       } <- get
                let (r, gtr') = mkRoundState gtr (length ps)
                let playersWithIndex = zip ps [0..]
                liftIO $ forM_ playersWithIndex (\(Player p, idx) -> initGameState (p, idx) (partialGame g{rounds=r:rs}) (cards . fromJust $ find ((== idx) .ownerIdx) (hands r)))
                put g{generator=gtr', rounds=r:rs}
                forM_ (take (length ps) ds) doBid
                g@Game {dealers=d:ds, rounds=r@Round{bids=bids}:rs} <- get
                let maxBid = maximumBy (comparing amount) bids
                let trump = fromJust $ bidSuit maxBid
                put g{dealers=ds, rounds=r{trump=trump}:rs}
                let bidder = ps !! bidderIdx maxBid
                liftIO $ forM_ playersWithIndex (\(Player p, idx) -> acknowledgeTrump (p, idx) maxBid (partialGame g) (cards . fromJust $ find ((== idx) .ownerIdx) (hands r)))
                tricks <- forM [1 .. 6] (const playTrick)
                liftIO $ print tricks
                let roundTalliesAndGame = map (tallyScore trump tricks) playersWithIndex
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

playGame :: Game (Player, Int)
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
    
    

data Player = forall p. (PlayerLogic p) => Player {getLogic :: p}

instance Show Player where
    show (Player p) = show p



