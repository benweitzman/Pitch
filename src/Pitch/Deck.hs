module Pitch.Deck (Deck
            	  ,deckSize
            	  ,newDeck
               	  ,shuffleDeck
            	  ,deal
            	  )
       
where       
  
import Pitch.Card  
import System.Random
  
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
                
deal :: Int -> Deck -> ([Card], Deck)        
deal n deck = (take n deck, drop n deck)
                         
  