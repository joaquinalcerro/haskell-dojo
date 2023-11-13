module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01

promptPlayer :: Player -> String
promptPlayer X = "Player X's turn: enter a row and column position (ex. A1)"
promptPlayer _ = "Player O's turn: enter a row and column position (ex. A1)"

-- Q#02

_RANGE_ :: [Int]
_RANGE_ = [ 1 .. _SIZE_ ]

-- Q#03

isDigit :: Char -> Bool
isDigit x = x `elem` ['0' .. '9'] 

readDigit :: Char -> Int
readDigit x 
  | isDigit x = read [x] 
  | otherwise = -1

-- Q#04

_EMPTY_ROW_ = replicate _SIZE_ E

_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied :: Board -> Bool
isTied [x, y, z] = notElem E x && notElem E y && notElem E z

-- all (== _EMPTY_ROW_) _EMPTY_BOARD_

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06

indexRowStrings :: [String] -> [(Char,String)]
indexRowStrings l = zip ['A'..] l

-- Q#07

formatLine :: [String] -> String
formatLine l = _SEP_ ++ intercalate _SEP_ l ++ _SEP_

-- Q#08

isMoveInBounds :: Move -> Bool
isMoveInBounds (x, y) =
  let xOK = elem x [0.._SIZE_]
      yOK = elem y [0.._SIZE_]
  in xOK && yOK

-- Q#09

stringToMove :: String -> Move
stringToMove [c, r] = (convertRowIndex c, readDigit r) 
stringToMove _ = _INVALID_MOVE_ 

-- Q#10

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow pValue iValue [] = [] 
replaceSquareInRow pValue iValue rRow 
  | iValue `notElem` [0 .. _SIZE_ - 1] = rRow
  | otherwise =
    let (l, r) = splitAt iValue rRow
    in l ++ pValue : tail r

rsX = replaceSquareInRow X
rsO = replaceSquareInRow O

