module Blackjack where

import Cards
import RunGame
--First Card
aCard1 :: Card
aCard1 = Card Ace Spades
--Second Card
aCard2 :: Card
aCard2 = Card Ace Hearts
--My Hand
aHands :: Hand
aHands = aCard1 : aCard2 : []
--Shows steps in size funktion
sizeSteps :: [Int]
sizeSteps = [size aHands
             , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
             ,size (Card Jack Spades : []) + 1 
             ,size ([]) + 2
             , 2]
--Shows a Hand of Cards
display :: Hand -> String
display [] = ""
display h = displaycard(head(h)) ++ ", " ++ display(tail(h))
--Shows a Single Card
displaycard :: Card -> String
displaycard c
    |length(show(rank c)) > 5 = (show(rank c) !! 8) : " of " ++ show(suit c)
displaycard c = show(rank c) ++ " of " ++ show(suit c)
--Shows the value of a hand
value :: Hand -> Int
value [] = 0
value h = valueCard(head(h)) + value(tail(h)) 
--Gives the value of diffrent ranks (Known bug with aces if more cards that equal 11 or more)
valueRank :: Rank -> Int
valueRank r 
    |length(show r) > 6 = read [(show(r) !! 8)] :: Int
    |length(show r) > 3 = 10
    |numberOfAces(aHands) <= 1 = 11
    |numberOfAces(aHands) > 1 = 1
valueCard :: Card -> Int
valueCard (Card r _) = valueRank(r)
--Shows the number of aces
numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces h 
    |show(rank(head(h))) == "Ace" = 1 + numberOfAces(tail(h))
    |otherwise = 0
--Is the player bust
gameOver :: Hand -> Bool
gameOver h = value(h) > 21
--Shows the winner of a round
winner :: Hand -> Hand -> Player
winner ph bh 
    |gameOver(ph) = Bank
    |gameOver(bh) = Guest
    |value(ph)>value(bh)  = Guest
    |value(ph)>value(bh)  = Bank
    |value(ph)==value(bh) = Bank