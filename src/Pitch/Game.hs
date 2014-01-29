{-# LANGUAGE ExistentialQuantification, RankNTypes, Rank2Types #-}

module Pitch.Game (mkRoundState
                  ,mkGameState
                  ,playGame
                  ,Player (..)
                  ,defaultPlayer
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
import qualified Pitch.Internal.Game as G
import           Pitch.Internal.PartialGame
import           Pitch.Internal.Common

type Pitch = StateT G.GameState IO 

newTrick :: Trick
newTrick = Trick [] (-1)

mkRoundState :: StdGen -> [PlayerId] -> (G.RoundState, StdGen)
mkRoundState g ps = (G.Round {G.deck = deck'
                             ,G.hands = zipWith Hand hs ps
                             ,G.bids = []
                             ,G.trump = minBound
                             ,G.tricks = []
                             }
                    ,g'
                    )
    where (hs, deck') = runState (forM ps (const $ deal 6)) d
          (d, g') = mkDeck g

partialRound :: G.RoundState -> RoundState
partialRound G.Round{G.bids=bids
                    ,G.trump=trump
                    ,G.tricks=tricks
                    } = 
  Round bids trump tricks

partialGame :: PlayerId -> G.GameState -> GameState
partialGame pid G.Game{G.scores=scores
                      ,G.players=players
                      ,G.rounds=rounds
                      } = 
  Game scores (map show players) (map partialRound rounds) hand
  where hand = cards $ fromMaybe (Hand [] pid) . find ((== pid) . ownerIdx) $ G.hands (head rounds)

wonBid :: Int -> G.RoundState -> Bool
wonBid n rs = bidderIdx (maximumBy (comparing amount) (G.bids rs)) == n
                                 
mkGameState :: StdGen -> [Player] -> G.GameState
mkGameState g ps = G.Game {G.players = playerList
                          ,G.scores = map (\(pid,_) -> (pid, 0)) playerList
                          ,G.generator = g
                          ,G.rounds = []
                          ,G.dealers = cycle playerList
                          }
  where playerList = zip [1..] ps

lastRound :: G.GameState -> G.RoundState
lastRound = head . G.rounds

checkForWinner :: Pitch (Maybe (PlayerId, Int))
checkForWinner = do gs@G.Game{G.scores=ss} <- get
                    if any ((>= 11) . snd) ss
                      then let overScores = filter ((>= 11) . snd) ss
                           in return $ find (\(widx, _) -> wonBid widx (lastRound gs)) overScores
                      else return Nothing

validateBidAmount :: PlayerId -> GameState -> Int -> Maybe String
validateBidAmount pid 
                  gs@(Game scores ps (r@(Round bids trump tricks):rs) hand)
                  x
  | x `notElem` [0, 2, 3, 4] = Just "A valid bid is 0 (pass), 2, 3, or 4"
  | x /= 0 && any (>= x) (map amount bids) = Just $ "You need to bid more than " ++ show (maximum $ map amount bids)
  | x == 0 && length bids + 1 == length ps && maximum (map amount bids) == 0 = Just "Since you are the last player to bid and everyone else has passed, you must bid at least 2"
  | otherwise = Nothing

validateBidSuit :: PlayerId  -> GameState -> Int -> Maybe Suit -> Maybe String
validateBidSuit _ _ x Nothing
  | x /= 0 = Just "You must specify a suit"
  | otherwise = Nothing
validateBidSuit pid Game{hand=hand} x (Just s) 
  | s `notElem` map suit hand = Just "You don't have any cards of that suit"
  | otherwise = Nothing

validateBid :: PlayerId -> GameState -> (Int, Maybe Suit) -> Maybe String
validateBid pid q (x, s) = getFirst $ First (validateBidAmount pid q x) <> First (validateBidSuit pid q x s)

validateCard :: PlayerId -> GameState -> Card -> Maybe String
validateCard pid 
             (Game scoresp ps (Round bids trump (Trick{played=played}:ts):rs) hand)
             c 
  | c `notElem` hand = Just "You don't have that card"
  | null ts && null played && suit c /= trump = Just "You must lead trump"
  | suit c /= trump
  && not (null played)
  && suit c /= suit (card (last played)) 
  && suit (card (last played)) `elem` map suit hand
  = Just "You must play trump or follow suit"
  | otherwise = Nothing

doBid :: (PlayerId, Player) -> Pitch ()
doBid (pid, p) = do g@G.Game{G.players=ps
                            ,G.rounds=r@G.Round{G.hands=hands
                                               ,G.bids=bs
                                               }:rs
                            } <- get
                    (amount, suit) <- liftIO $ (mkBid p) pid $ partialGame pid g
                    let newBid = Bid amount suit pid
                        newState = g{G.rounds=r{G.bids=newBid:bs}:rs}
                    put newState
                    liftIO $ forM_ ps (\(pid, p) -> (postBidHook p) pid newBid $ partialGame pid newState)

doPlay :: (PlayerId, Player) -> Pitch ()
doPlay (pid, p) = do g@G.Game{G.players=ps
                             ,G.rounds=r@G.Round{G.hands=hands
                                                ,G.tricks=t@Trick{played=played}:ts}:rs
                             } <- get
                     card <- liftIO $ (mkPlay p) pid $ partialGame pid g
                     let newPlay = Play card pid
                         newGS = g{G.rounds=r{G.tricks=t{played=newPlay:played}:ts
                                             ,G.hands=removeCard card hands
                                             }:rs
                                  }
                     put newGS
                     liftIO $ forM_ ps (\(pid, p) -> (postPlayHook p)  pid newPlay $ partialGame pid newGS)
  where removeCard c [] = []
        removeCard c (h@Hand{cards=cs,ownerIdx=i}:hs) | i == pid = h{cards=delete c cs}:hs
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

tallyScore :: Suit -> [Trick] -> (PlayerId, Player) -> (PlayerId, (Int, Int))
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
    in (pid, (hi + lo + jack, game))

gamePoints :: Card -> Int
gamePoints (Card r _) = rankToGame r
    where rankToGame Jack = 1
          rankToGame Queen = 2
          rankToGame King = 3
          rankToGame Ace = 4
          rankToGame (Number 10) = 10
          rankToGame _ = 0

playTrick :: Pitch Trick
playTrick = do g@G.Game {G.rounds=r@G.Round{G.bids=bids
                                           ,G.tricks=ts
                                           }:rs
                        ,G.dealers=ds
                        ,G.players=ps
                        } <- get
               let startingPlayerIdx = if null ts
                                       then bidderIdx $ maximumBy (comparing amount) bids
                                       else winnerIdx $ head ts
               put g{G.rounds=r{G.tricks=newTrick:ts}:rs}
               let playOrder = take (length ps) $ dropWhile ((/= startingPlayerIdx) . fst) (cycle ps)
               forM_ playOrder doPlay
               g@G.Game {G.rounds=r@G.Round{G.tricks=t:ts
                                           ,G.trump=trump
                                           }:rs
                        } <- get
               let t' = trickWinner trump t
               put g{G.rounds=r{G.tricks=t':ts}:rs}
               return t'

playRound :: Pitch ()
playRound = do  liftIO $ putStrLn "playing a round"
                g@G.Game {G.players=ps
                         ,G.generator=gtr
                         ,G.rounds=rs
                         ,G.dealers=ds
                         ,G.scores=ss
                         } <- get
                lift $ print g
                let (r, gtr') = mkRoundState gtr (map fst ps)
                    newState = g{G.rounds=r:rs
                                ,G.generator=gtr'
                                }
                lift $ print newState
                liftIO $ forM_ ps (\(pid, p) -> (initGameState p) pid $ partialGame pid newState)
                put newState
                forM_ (take (length ps) ds) doBid
                g@G.Game {G.dealers=d:ds
                         ,G.rounds=r@G.Round{G.bids=bids}:rs
                         } <- get
                let maxBid = maximumBy (comparing amount) bids
                    trump = fromJust $ bidSuit maxBid
                put g{G.dealers=ds
                     ,G.rounds=r{G.trump=trump}:rs
                     }
                liftIO $ forM_ ps (\(pid, p) -> (acknowledgeTrump p) pid maxBid $ partialGame pid g)
                tricks <- forM [1 .. 6] (const playTrick)
                let roundTalliesAndGame = map (tallyScore trump tricks) ps
                    roundTallies = map (\(pid, (pts, game)) -> if game == maximum (map (snd . snd) roundTalliesAndGame)
                                                               then (pid, pts + 1)
                                                               else (pid, pts)) 
                                       roundTalliesAndGame
                    roundScores = map (\(pid, score) -> if pid /= bidderIdx maxBid
                                                           || score >= amount maxBid
                                                        then score
                                                        else (-1 * amount maxBid))
                                      roundTallies
                    newScores = zipWith (\(pid, s1) s2 -> (pid, s1 + s2)) ss roundScores
                liftIO . putStr $ "Scores: " ++ show newScores
                g <- get
                put g{G.scores=newScores}                              
                return ()

playGame :: Pitch (PlayerId, Int)
playGame = do playRound
              maybeWinner <- checkForWinner
              let winnerSt = case maybeWinner of
                                Nothing -> playGame
                                Just x -> return x
              (winP, score) <- winnerSt
              liftIO $ putStrLn "we have a winner"
              liftIO $ print score
              winnerSt



