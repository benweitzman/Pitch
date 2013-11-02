{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Card (Suit (..)
            ,Rank (..)
            ,Card (..)
            )
       
where
  
import Control.Applicative  
import Data.List  

data Suit = Clubs | Spades | Hearts | Diamonds deriving (Eq, Read, Show, Enum, Bounded)

data Rank = Jack
          | Queen
          | King
          | Ace
          | Number Int deriving (Eq, Read, Show)
  
                                
instance Ord Rank where
  Number i <= Number j = i <= j
  _ <= Number _ = False
  Jack <= _ = True
  Queen <= Jack = False
  Queen <= _ = True
  King <= Jack = False
  King <= Queen = False
  King <= _ = True
  Ace <= Ace = True
  Ace <= _ = False
  
instance Bounded Rank where
  minBound = Number 2
  maxBound = Ace
  
cardOrder = (Number <$> [2..10]) ++ [Jack, Queen, King, Ace]

instance Enum Rank where
  succ (Number 10) = Jack
  succ (Number n) = Number (n + 1)
  succ Jack = Queen
  succ Queen = King
  succ King = Ace
  succ Ace = error "No rank greater than ace"
  
  pred (Number 2) = error "No rank lower than 2"
  pred (Number n) = Number (n - 1)
  pred Jack = Number 10
  pred Queen = Jack
  pred King = Queen
  pred Ace = King
    
  toEnum n = cardOrder !! n
  fromEnum r = case elemIndex r cardOrder of
                 Nothing -> error "no valid rank for index"
                 Just n -> n
  
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where bound | fromEnum y >= fromEnum x = maxBound
                | otherwise = minBound
                              
data Card = Card { rank :: Rank, suit :: Suit} deriving (Eq, Show, Read, Bounded)                               

cards = [Card r s | s <- [minBound .. ], r <- [minBound .. ]]

instance Enum Card where
  toEnum n = cards !! n
  fromEnum c = case elemIndex c cards of
                 Nothing -> error "no valid card for index"
                 Just n -> n
                 
  enumFrom x = enumFromTo x maxBound
  
  