module Pitch.Players.Local (mkPlayer)

where       
       
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Ord
import Pitch.Game            
import Pitch.Parser
import System.IO
       
  
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

mkPlayer :: String -> Player
mkPlayer name = defaultPlayer {
    mkBid = \pid
             gs@(Game scores players (Round bids trump tricks:rs) hand)
             ->
        do liftIO $ putStrLn (name ++ ", it is your turn to bid")
           if all (==0) (map amount bids)
              then liftIO $ putStrLn "Nobody has bid yet"
              else let maxBid = maximumBy (comparing amount) bids
                       bidder = players !! bidderIdx maxBid
                   in liftIO $ putStrLn (bidder ++ " has bid " ++ show (amount maxBid))
           liftIO $ putStr "Your hand is "
           liftIO $ print hand
           bid <- validatePrompt "What is your bid?" 
                                 parseInt $
                                 validateBidAmount pid gs
           suit <- if bid /= 0
                   then do s <- validatePrompt "What suit?" 
                                               parseSuit $
                                               validateBidSuit pid gs bid . Just
                           return (Just s)
                   else return Nothing
           return (bid, suit)
    ,
    mkPlay = \pid 
              gs@(Game scoregs 
                       players 
                       (Round bids 
                              trump
                              (Trick{played=played}:ts)
                        :rs)
                       hand)
              ->
        do liftIO $ putStrLn (name ++ ", it is your turn to play")
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
                          validateCard pid gs
    ,
    name = name
}

  