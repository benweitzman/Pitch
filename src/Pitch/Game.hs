{-# LANGUAGE ExistentialQuantification, RankNTypes, Rank2Types #-}

module Pitch.Game (mkRoundState
                  ,mkGameState
                  ,playGame
                  ,Player (..)
                  ,Hand (..)
                  ,Bid (..)
                  ,Play (..)
                  ,Trick (..)
                  ,validateBidAmount
                  ,validateBidSuit
                  ,validateCard
                  ,validateBid
                  ,GameState (..)
                  ,RoundState (..)
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
import qualified Pitch.Internal as I (GameState (..), RoundState (..), Pitch) 
import           Pitch.Internal hiding (GameState, RoundState, Pitch)

type RoundState = PartialRoundState
type GameState = PartialGameState

newTrick :: Trick
newTrick = Trick [] (-1)

mkRoundState :: StdGen -> Int -> (I.RoundState, StdGen)
mkRoundState g ps = (I.Round {deck = deck'
                             ,hands = zipWith Hand hs [0..]
                             ,bids = []
                             ,trump = minBound
                             ,tricks = []
                             }
                    ,g'
                    )
    where (hs, deck') = runState (forM [0..ps-1] (const $ deal 6)) d
          (d, g') = mkDeck g

partialRound :: I.RoundState -> RoundState
partialRound I.Round{bids=bids,trump=trump,tricks=tricks} = 
  PartialRound bids trump tricks

partialGame :: I.GameState -> GameState
partialGame I.Game{scores=scores,players=players,rounds=rounds} = 
  PartialGame scores (map show players) (map partialRound rounds)

wonBid :: Int -> I.RoundState -> Bool
wonBid n rs = bidderIdx (maximumBy (comparing amount) (bids rs)) == n
                                 
mkGameState :: StdGen -> [Player] -> I.GameState
mkGameState g ps = I.Game {players = playerList
                          ,scores = map (\(pid,_) -> (pid, 0)) playerList
                          ,generator = g
                          ,rounds = []
                          ,dealers = cycle playerList
                          }
  where playerList = zip [1..] ps

lastRound :: I.GameState -> I.RoundState
lastRound = head . rounds

checkForWinner :: I.Pitch (Maybe (PlayerId, Int))
checkForWinner = do gs@I.Game{scores=ss} <- get
                    if any ((>= 11) . snd) ss
                      then let overScores = filter ((>= 11) . snd) ss
                           in return $ find (\(widx, _) -> wonBid widx (lastRound gs)) overScores
                      else return Nothing

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
  | suit c /= trump
  && not (null played)
  && suit c /= suit (card (last played)) 
  && suit (card (last played)) `elem` map suit hand
  = Just "You must play trump or follow suit"
  | otherwise = Nothing

doBid :: (PlayerId, Player) -> I.Pitch ()
doBid (idx, p) = do g@I.Game{players=ps,rounds=r@Round{hands=hands}:rs} <- get
                    (amount, suit) <- liftIO $ (mkBid p) idx $ partialGame g
                    g@Game {rounds=r@Round{bids=bs}:rs} <- get
                    let newBid = Bid amount suit idx
                    put g{rounds=r{bids=newBid:bs}:rs}
                    liftIO $ forM_ ps (\(pid, p) -> (postBidHook p) pid newBid $ partialGame g{rounds=r{bids=newBid:bs}:rs})

doPlay :: (PlayerId, Player) -> I.Pitch ()
doPlay (idx, p) = do g@I.Game{players=ps,rounds=r@Round{hands=hands}:rs} <- get
                     card <- liftIO $ (mkPlay p) idx $ partialGame g
                     g@Game {rounds=r@Round{tricks=t@Trick{played=played}:ts,hands=hands}:rs} <- get
                     let newPlay = Play card idx
                         newGS = g{rounds=r{tricks=t{played=newPlay:played}:ts
                                           ,hands=removeCard card hands
                                           }:rs}
                     put newGS
                     liftIO $ forM_ ps (\(pid, p) -> (postPlayHook p)  pid newPlay $ partialGame newGS)
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

tallyScore :: Suit -> [Trick] -> (PlayerId, Player) -> (Int, Int)
tallyScore trump tricks (pid, _) = 
    let tricksWon = filter ((== pid) . winnerIdx) tricks
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

playTrick :: I.Pitch Trick
playTrick = do g@I.Game {rounds=r@Round{bids=bids, tricks=ts}:rs, dealers=ds, players=ps} <- get
               let startingPlayerIdx = if null ts
                                       then bidderIdx $ maximumBy (comparing amount) bids
                                       else winnerIdx $ head ts
               put g{rounds=r{tricks=newTrick:ts}:rs}
               let playOrder = take (length ps) $ dropWhile ((/= startingPlayerIdx) . fst) (cycle ps)
               forM_ playOrder doPlay
               g@Game {rounds=r@Round{tricks=t:ts, trump=trump}:rs} <- get
               let t' = trickWinner trump t
               put g{rounds=r{tricks=t':ts}:rs}
               return t'

playRound :: I.Pitch ()
playRound = do  liftIO $ putStrLn "playing a round"
                g@I.Game {players=ps
                         ,generator=gtr
                         ,rounds=rs
                         ,dealers=ds
                         ,scores=ss
                         } <- get
                let (r, gtr') = mkRoundState gtr (length ps)
                liftIO $ forM_ ps (\(pid, p) -> (initGameState p) pid $ partialGame g{rounds=r:rs})
                put g{generator=gtr', rounds=r:rs}
                forM_ (take (length ps) ds) doBid
                g@Game {dealers=d:ds, rounds=r@Round{bids=bids}:rs} <- get
                let maxBid = maximumBy (comparing amount) bids
                    trump = fromJust $ bidSuit maxBid
                put g{dealers=ds, rounds=r{trump=trump}:rs}
                let bidder = ps !! bidderIdx maxBid
                liftIO $ forM_ ps (\(pid, p) -> (acknowledgeTrump p) pid maxBid $ partialGame g)
                tricks <- forM [1 .. 6] (const playTrick)
                let roundTalliesAndGame = map (tallyScore trump tricks) ps
                    roundTallies = map (\(pts, game) -> if game == maximum (map snd roundTalliesAndGame)
                                                        then pts + 1
                                                        else pts) roundTalliesAndGame
                    roundScores = mapWithIndex (\(idx, score) -> if idx /= bidderIdx maxBid
                                                                    || roundTallies !! idx >= amount maxBid
                                                                 then score
                                                                 else (-1 * amount maxBid))
                                               roundTallies
                    newScores = zipWith (\(pid, s1) s2 -> (pid, s1 + s2)) ss roundScores
                liftIO . putStr $ "Scores: " ++ show newScores
                put g{scores=newScores}                              
                return ()

playGame :: I.Pitch (PlayerId, Int)
playGame = do playRound
              maybeWinner <- checkForWinner
              let winnerSt = case maybeWinner of
                                Nothing -> playGame
                                Just x -> return x
              (winP, score) <- winnerSt
              liftIO $ putStrLn "we have a winner"
              liftIO $ print score
              winnerSt



