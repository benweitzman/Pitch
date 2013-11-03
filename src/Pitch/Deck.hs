module Pitch.Deck (Deck
                  ,deckSize
                  ,newDeck
                  ,shuffleDeck
                  ,mkDeck
                  ,deal
                  )
       
where       
  
import Pitch.Card  
import System.Random
import Control.Monad.State
import Data.List
import Data.Ord
  
type Deck = [Card]

newDeck :: Deck
newDeck = [minBound .. ]

deckSize :: Int 
deckSize = 52

shuffleDeck :: RandomGen g => g -> Deck -> (Deck, g)
shuffleDeck generator [] = ([], generator)
shuffleDeck generator deck = let (idx, generator') = randomR (0, length deck -1) generator
                                 (x, xs) = removeAt idx deck
                                 (xs', generator'') = shuffleDeck generator' xs
                             in (x:xs', generator'')
  where removeAt pos deck = (deck !! pos, take pos deck ++ drop (pos + 1) deck)

mkDeck :: RandomGen g => g -> (Deck, g)
mkDeck generator = shuffleDeck generator newDeck
         
deal :: Int -> State Deck [Card]      
deal n = state $ \xs -> (sortBy (comparing suit) (sortBy (comparing rank) $ take n xs), drop n xs)


                         
  