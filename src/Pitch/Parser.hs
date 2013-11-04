{-# LANGUAGE OverloadedStrings #-}

module Pitch.Parser(parseCard)
       
where
  
import Data.Attoparsec.Text
import Data.Text
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
rankParser = (char '2' >> return (Number 2))
             <|> (char '3' >> return (Number 3))
             <|> (char '4' >> return (Number 4))
             <|> (char '5' >> return (Number 5))
             <|> (char '6' >> return (Number 6))
             <|> (char '7' >> return (Number 7))
             <|> (char '8' >> return (Number 8))
             <|> (char '9' >> return (Number 9))
             <|> (string "10" >> return (Number 10))
             <|> (string "Ten" >> return (Number 10))
             <|> (string "Jack" >> return Jack)
             <|> (string "Queen" >> return Queen)
             <|> (string "King" >> return King)
             <|> (string "Ace" >> return Ace)
             <|> (char 'T' >> return (Number 10))
             <|> (char 'J' >> return Jack)
             <|> (char 'Q' >> return Queen)
             <|> (char 'K' >> return King)
             <|> (char 'A' >> return Ace)
             
cardParser :: Parser Card
cardParser = (do rank <- rankParser
                 suit <- suitParser
                 endOfInput
                 return $ Card rank suit)
         <|> (do rank <- rankParser
                 string " of "
                 suit <- suitParser
                 endOfInput
                 return $ Card rank suit)
  
parseCard :: String -> Maybe Card
parseCard s = case parseOnly cardParser (pack s) of
                Left _-> Nothing
                Right c -> Just c
                   
