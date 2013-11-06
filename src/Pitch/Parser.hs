{-# LANGUAGE OverloadedStrings #-}

module Pitch.Parser(parseCard
                   ,parseSuit
                   )
       
where
  
import Data.Attoparsec.Text
import Data.Text (pack)
import Control.Applicative
import Pitch.Card

suitParser :: Parser Suit
suitParser = (string "Clubs" >> return Clubs) 
         <|> (string "Spades" >> return Spades)
         <|> (string "Diamonds" >> return Diamonds) 
         <|> (string "Hearts" >> return Hearts)
         <|> (char 'S' >> return Spades)
         <|> (char 'D' >> return Diamonds) 
         <|> (char 'H' >> return Hearts)
         <|> (char 'C' >> return Clubs) 

rankParser :: Parser Rank
rankParser = foldr1 (<|>) $ map (\(pattern, value) -> string pattern >> return value)
             [("2", Number 2)
             ,("3", Number 3)
             ,("4", Number 4)
             ,("5", Number 5)
             ,("6", Number 6)
             ,("7", Number 7)
             ,("8", Number 8)
             ,("9", Number 9)
             ,("10", Number 10)
             ,("Ten", Number 10)
             ,("Jack", Jack)
             ,("Queen", Queen)
             ,("King", King)
             ,("Ace", Ace)
             ,("T", Number 10)
             ,("J", Jack)
             ,("Q", Queen)
             ,("K", King)
             ,("A", Ace)
             ]

cardParser :: Parser Card
cardParser = (do rank <- rankParser
                 parseWithRank rank)
         <|> (do rank <- rankParser
                 string " of "
                 parseWithRank rank)
    where parseWithRank rank = do suit <- suitParser
                                  endOfInput
                                  return $ Card rank suit         
  
parseCard :: String -> Maybe Card
parseCard s = case parseOnly cardParser (pack s) of
                Left _-> Nothing
                Right c -> Just c
                   
parseSuit :: String -> Maybe Suit
parseSuit s = case parseOnly suitParser (pack s) of
                Left _ -> Nothing
                Right x -> Just x