--  File     : Cardguess.hs
--  Author   : Hao Wang 654727
--  Purpose  : Cardguess program for cardguess project

module Cardguess (
	initialGuess, 
	nextGuess, 
	GameState ) 
where

import Card
import Data.List
import Data.Function

type GameState = [[Card]]

-- Function cardStore 
-- return the solution space based on the number of card in game 
-- for example, if input 3, return all the possible combinations of card
-- to be used as solution set in gameState

cardStore :: Int -> GameState
cardStore number 
     | number == 1 = [[a] | a <- [minBound..maxBound] :: [Card]]
     | number == 2 = [[a,b] | a <- [minBound..maxBound] :: [Card], b <- [minBound..maxBound] :: [Card], a < b]
     | number == 3 = [[a, b, c] | a <- [minBound..maxBound] :: [Card], b <- [minBound..maxBound] :: [Card], c <- [minBound..maxBound] :: [Card], a < b && c < b]
     | number == 4 = [[a, b, c, d] | a <- [minBound..maxBound] :: [Card], b <- [minBound..maxBound] :: [Card], c <- [minBound..maxBound] :: [Card], d <- [minBound..maxBound] :: [Card], a < b && b < c && c < d]

-- Function initialGuess 
-- return the first guess & its gameState, based on the inputed card number
-- for exampe input 3, reture a guess of 3 cards

initialGuess :: Int -> ([Card],GameState)
initialGuess number 
     |number < 1  = error " no guess cards"
     |number == 1 = ([Card Club R8], (cardStore number) )
     |number == 2 = ([Card Club R4, Card Heart R10], (cardStore number) )
     |number == 3 = ([Card Club R4, Card Diamond R7,Card Heart R10], (cardStore number))
     |number == 4 = ([Card Club R4, Card Diamond R6,Card Heart R8,Card Spade R10], (cardStore number) )


-- Function nextGuess 
-- return the subsequent guesses & its gameState, based on inputed feedback
-- for every feedback, filter out those guesses in solution space which has different feedback with inputed one
-- and choose the last cardset in the filtered solution space, aka cardstore, as the next guess 

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (currentGuess, cardstore) feedback = (nextGuess, newAnswers)
      where  
 		   newAnswers = filter (falseCardFilter currentGuess feedback) cardstore
 		   nextGuess  = last newAnswers

-- Function falseCardFilter 
-- return a function that is used in `nextGuess`, for filtering out those guesses whose feedback are not same as the feedback
-- for every feedback, filter out those guesses in solution space which has different feedback with inputed one
-- and choose the last cardset in the filtered solution space, aka cardstore, as the next guess 

falseCardFilter :: [Card] ->  (Int,Int,Int,Int,Int) -> [Card] -> Bool
falseCardFilter [] _ [] = False
falseCardFilter lastGuess feedback curGuess = getFeedback lastGuess curGuess == feedback

-- Function getFeedback 
-- return a tuple that represent the feedback
-- based on the previous inputed card guess and current guess

getFeedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
getFeedback pguess [] = error "no guess in the List"
getFeedback pguess rguess = (n1,n2,n3,n4,n5)
	where 
	  	n1 = length $ filter (`elem` rguess) pguess
	  	n2 = length $ filter (< (minimum $ (map rank pguess))) (map rank rguess)
	  	n3 = length $ selectSame (map rank pguess) (map rank rguess)	
	  	n4 = length $ filter (> (maximum $  (map rank pguess))) (map rank rguess)
	  	n5 = length $ selectSame (map suit rguess) (map suit pguess)
 

-- Function selectSame
-- intersect the two inputed lists , return a list of result
-- for example input [1,1] [1,2], return [1]
-- called in getFeedback function

selectSame :: (Eq a) => [a] -> [a] -> [a]
selectSame [] _ = []
selectSame (card:preGuess) curGuess 
	| card `elem` curGuess = card : selectSame preGuess (delete card curGuess)
	| otherwise = selectSame preGuess curGuess


           	
