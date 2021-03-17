-- CptS 355 - Spring 2021 -- Homework1 - Haskell
-- Name: Setenay Guner
-- Collaborators: Went to Sakire's office hours and got help from TAs

module HW1
     where

-- Q1(a) getUniqueRight

getUniqueRight:: Eq a => [a] -> [a]
getUniqueRight [] = []
getUniqueRight (x:xs) | x `elem` xs = (getUniqueRight xs)
                      | otherwise = x:(getUniqueRight xs)
--elem 1 [1,2,3,4,5] 


-- Q1(b) getUniqueLeft

getUniqueLeft:: Eq a => [a] -> [a]
getUniqueLeft [] = []
getUniqueLeft (x:xs) | (last (x:xs)) `elem` (init(x:xs)) = (getUniqueLeft (init(x:xs)))
                     | otherwise = (getUniqueLeft (init(x:xs))) ++ [last (x:xs)]

{- 1: 2: 4: []
[1,2,1,4,2,4]

[4,2,1]
[1,2,4] -}

-- Q2(a) cansInLog
--return num  of cans consumed
cansInLog [] = 0  --takes a list returns an integer
cansInLog ((x,y):xs) = y + (cansInLog xs)
-- -- x is flavor and y is num of cans 

-- Q2(b) numCans
-- (((month, year), ((food, cans):monthlog)):rest)
numCans [] n = 0
numCans (((month,year),log):xs) n = if year == n  then  (cansInLog log) + (numCans xs n) else numCans xs n


-- Q2(c) getMonths
getMonths :: (Ord t1, Num t1, Eq t2) => [((a, b), [(t2, t1)])] -> t1 -> t2 -> [(a, b)]
getMonths [] n flavor = []
getMonths (x:xs) n flavor | (helper(snd x) flavor) > n = (fst x) : (getMonths xs n flavor)
                          | otherwise = (getMonths xs n flavor)
                    
helper [] flavor = 0
helper (x:xs) flavor | (fst x) == flavor = (snd x) 
                     | otherwise = (helper xs flavor)

-- Q3 deepCount

deepCount :: (Num p, Eq t) => t -> [[t]] -> p
deepCount v [] = 0
deepCount v (x:xs) = (listhelper x v) + (deepCount v xs)

listhelper [] looking = 0
listhelper (x:xs) looking
                         | (x == looking) = 1 + (listhelper xs looking)
                         | otherwise = (listhelper xs looking)

-- Q4 clusterConsecutive
-- clusterConsecutive [1,2,3,5,6,7,8,9,2,3,11,12]  ->   [[1,2,3],[5,6,7,8,9],[2,3],[11,12]
