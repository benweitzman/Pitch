module Pitch.Card (Suit (..)
                  ,Rank (..)
                  ,Card (..)
                  )

where

import Control.Applicative
import Data.List
import Data.Maybe

data Suit = Clubs | Spades | Hearts | Diamonds deriving (Eq, Read, Enum, Bounded, Ord)

instance Show Suit where
  show Clubs    = "C"
  show Spades   = "S"
  show Hearts   = "H"
  show Diamonds = "D"


data Rank = Jack
          | Queen
          | King
          | Ace
          | Number Int deriving (Eq)

instance Show Rank where
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"
  show (Number 10) = "T"
  show (Number n) = show n


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
  Number _ <= _ = True

instance Bounded Rank where
  minBound = Number 2
  maxBound = Ace

cardOrder :: [Rank]
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
  fromEnum r = fromMaybe (error "no valid rank for index") (elemIndex r cardOrder)

  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where bound | fromEnum y >= fromEnum x = maxBound
                | otherwise = minBound

data Card = Card { rank :: Rank, suit :: Suit} deriving (Eq, Bounded)

cards :: [Card]
cards = [Card r s | s <- [minBound .. ], r <- [minBound .. ]]

instance Enum Card where
  toEnum n = cards !! n
  fromEnum c = fromMaybe (error "no valid card for index") (elemIndex c cards)

  enumFrom x = enumFromTo x maxBound

instance Show Card where
  show (Card r s) = show r ++ show s
