module TTT.A3 where
import Debug.Trace 


import Data.List (transpose)
import TTT.A1
import TTT.A2

-- Q#01

showInts :: [Int] -> [String]
showInts [] = [] 
showInts (x : xs) = show x : showInts xs

_HEADER_ = ' ' : formatLine (showInts _RANGE_) 

-- Q#02

showSquares :: [Square] -> [String]
showSquares [] = []
showSquares (x : xs) = showSquare x : showSquares xs 

-- Q#03

formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (x : xs) = formatLine (go [] x) : formatRows xs
  where 
    go acc [] = reverse acc
    go acc ( y : ys) = go (showSquare y : acc) ys

-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty (h:_) 0 = if h == E then True else False
isColEmpty (x:xs) i = isColEmpty xs (i - 1)

-- Q#05

dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol (x:xs) = tail x : dropFirstCol xs

dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (x:xs) = init x : dropLastCol xs

-- Q#06

getDiag1 :: Board -> Line
getDiag1 [] = [] 
getDiag1 xs = goB 0 [] xs  
  where
    goB _ acc [] = reverse acc
    goB i acc (y:ys) = goB (i + 1) ((y !! i) : acc) ys

getDiag2 :: Board -> Line
getDiag2 [] = [] 
getDiag2 xs = goB (_SIZE_ - 1) [] xs  
  where
    goB _ acc [] = reverse acc
    goB i acc (y:ys) = goB (i - 1) ((y !! i) : acc) ys

-- Listado de funciones a ejecutar
--
getHorizLines :: Board -> [Line]
getHorizLines b = go [] b 
  where
    go acc [] = reverse acc
    go acc (x:xs) = go (x : acc) xs

getVerticalLines :: Board -> [Line]
getVerticalLines board = transpose board

getAllLines :: Board -> [Line]
getAllLines board = getVerticalLines board ++ getHorizLines board ++ [getDiag1 board] ++ [getDiag2 board]  

-- Q#07

putSquare :: Player -> Board -> Move -> Board
putSquare player [] move = []
putSquare player board (rowIndex, colIndex) = go board rowIndex 0 []
  where
    go :: Board -> Int -> Int -> [Row] -> Board
    go [] _ _ newBoard = newBoard
    go (row:xs) rowIndex index newBoard = 
      if rowIndex == index
        then
          newBoard ++ (replaceSquareInRow player colIndex row : xs) 
        else
          go xs rowIndex (index + 1) (row : newBoard)
    
-- Q#08

prependRowIndices :: [String] -> [String]
prependRowIndices list = let
  newList = indexRowStrings list
  in
    go newList []
    where
      go []     acc = reverse acc
      go ((a, b):xs) acc = go xs ((a : b) : acc)  
    
-- Q#09

isWinningLine :: Player -> Line -> Bool
isWinningLine _ [] = False
isWinningLine player line = go line False
  where
    go []     acc = acc
    go (x:xs) acc
      | x == player = go xs True
      | otherwise = go [] False

-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove board move = let
  inBound = isMoveInBounds move
  in if inBound
     then go board move 0 
     else False
     where 
       go :: Board -> Move -> Int -> Bool
       go [] _ _ = False
       go (line:xs) (x, y) index  
         | y == index = isColEmpty line x 
         | otherwise = go xs (x, y) (index + 1) 
