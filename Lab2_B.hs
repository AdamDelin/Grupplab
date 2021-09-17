{- Lab 2 B
Authors: Adam Delin, Christine Pettersson, Danilo Vergara   
Lab group: 52 -}    
module Blackjack where

import Cards
import RunGame
implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation
--First Card
aCard1 :: Card
aCard1 = Card (Numeric 4) Spades
--Second Card
aCard2 :: Card
aCard2 = Card Ace Hearts
--Third Card
aCard3 :: Card
aCard3 = Card (Numeric 7) Spades
--Fourth Card
aCard4 :: Card
aCard4 = Card Queen Hearts
--My Hand
aHands :: Hand
aHands = aCard1 : aCard2 : aCard3 : []
--Bank Hand
aHands2 :: Hand
aHands2 = aCard3 : aCard4 : []
--Shows steps in size funktion
sizeSteps :: [Int]
sizeSteps = [size aHands, size (Card (Numeric 2) Hearts : (Card Jack Spades : [])),size (Card Jack Spades : []) + 1,size ([]) + 2, 2]
--Shows a Hand of Cards
display :: Hand -> String
display [] = ""
display h = displaycard(head(h)) ++ ", " ++ display(tail(h))
--Shows a Single Card
displaycard :: Card -> String
displaycard c    
    |length(show(rank c)) == 10 = (show(rank c) !! 8) : (show(rank c) !! 9) : " of " ++ show(suit c)    
    |length(show(rank c)) > 5 = (show(rank c) !! 8) : " of " ++ show(suit c)
displaycard c = show(rank c) ++ " of " ++ show(suit c)
--Shows the value of a hand
value :: Hand -> Int
value h    
    |value2(h) > 21 && numberOfAces(h) > 0 = value2(h) - (numberOfAces(h)*10)    
    |otherwise = value2(h)
value2 :: Hand -> Int
value2 [] = 0
value2 h = valueCard(head(h)) + value2(tail(h))  
--Gives the value of diffrent ranks
valueRank :: Rank -> Int
valueRank r     
    |length(show r) == 9 = read [(show(r) !! 8)] :: Int
    |length(show r) > 3 = 10    
    |otherwise = 11
valueCard :: Card -> Int
valueCard (Card r _) = valueRank(r)
--Shows the number of aces
numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces h     
    |show(rank(head(h))) == "Ace" = 1 + numberOfAces(tail(h))    
    |otherwise = 0 + numberOfAces(tail(h))
--Is the player bust
gameOver :: Hand -> Bool
gameOver h = value(h) > 21
--Shows the winner of a round
winner :: Hand -> Hand -> Player
winner ph bh     
    |gameOver(ph) = Bank    
    |gameOver(bh) = Guest    
    |value(ph)>value(bh)  = Guest    
    |value(ph)<value(bh)  = Bank    
    |value(ph)==value(bh) = Bank
--New Deck of Cards
fullDeck :: Deck
fullDeck = testDeck allranks allsuits
--Funktion for deck of Cards
testDeck :: [Rank] -> [Suit] -> Deck
testDeck r s
    |null s = []
    |null r = testDeck allranks (tail s)
    |otherwise = Card (head r) (head s) : testDeck (tail r) s 
--List of all ranks
allranks :: [Rank]
allranks = map Numeric [2..10] ++ Jack : Queen : King : Ace : []
--List of all suits
allsuits :: [Suit]
allsuits = [Hearts,Spades,Diamonds,Clubs]
--Takes a Card from the deck and adds it to a hand
draw :: Deck -> Hand -> (Deck, Hand)
draw d h 
    |null d = error "draw: The deck is empty"
draw d h = (tail d, (head d : h))
--Plays a round for the Bank
playBank :: Deck -> Hand
playBank d = playBank' d bankHand
--Helper funktion to playBank
playBank' :: Deck -> Hand -> Hand
playBank' deck bankHand
    |value bankHand < 16 = playBank' (fst(deck' , bankHand')) (snd(deck' , bankHand'))
    |otherwise = bankHand
    where (deck', bankHand') = draw deck bankHand
--Guest Hand
guestHand :: Hand
guestHand = []
--Bank Hand
bankHand :: Hand
bankHand = []
--Shuffles a new Deck 
shuffle :: [Double] -> Deck -> Deck
shuffle du deck
    |null du = deck
    |head du > 0.9 = shuffle (tail du) (shuffle' 5 deck)
    |head du > 0.8 = shuffle (tail du) (cut deck)
    |head du > 0.5 = shuffle (tail du) (shuffle' 2 deck)
    |head du > 0.1 = shuffle (tail du) (cut (shuffle' 3 deck))
    |head du > 0.001 = shuffle (tail du) deck 
    |head du > 0.0001 = shuffle (tail du) (cut(shuffle' 7 deck))
    |otherwise = cut(shuffle' 4 deck)
--Riffles the Cards n times
shuffle' :: Int -> Deck -> Deck
shuffle' 0 deck = deck
shuffle' n deck = shuffle' (n-1) (newshuffle top bot)
    where 
        top = fst(splitAt 26 deck)
        bot = snd(splitAt 26 deck)
--puts the bottom half on top
cut :: Deck -> Deck
cut deck = bot ++ top

    where 
        top = fst(splitAt 26 deck)
        bot = snd(splitAt 26 deck)
--riffles one time
newshuffle :: Deck -> Deck -> Deck
newshuffle deck dec 
    |null deck = []
    |otherwise = [head deck] ++ [head dec] ++ newshuffle (tail deck)  (tail dec) 
    


--Checks if a card is still in the deck after shuffle
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

--QuickCheck tests on Deck
prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = 
    size deck == size (shuffle randomlist deck)