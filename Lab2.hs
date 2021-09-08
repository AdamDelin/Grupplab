module Blackjack where

import Cards
import RunGame
--First Card
aCard1 :: Card
aCard1 = Card (Numeric 2) Spades
--Second Card
aCard2 :: Card
aCard2 = Card King Hearts
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

display :: Hand -> String
display = undefined 

displaycard :: Card -> String
displaycard = undefined

