import System.IO
import Data.Char
import Data.List
import Data.Maybe

--Define types for an easier reading

-- [---|---|---] -> Line  }
-- [-S-|-S-|-S-]          } => Vector
-- [---|---|---]          }
-- ----|---|----
-- [---|---|---] -> Line
-- [-S-|-S-|-S-]
-- [---|---|---]
-- ----|---|----
-- [---|---|---] -> Line
-- [---|---|---]
-- [---|---|---]
--   ^
--   Column

type Line = [Int]
type Column = [Int]
type Vector = [Square]
type Square = [Line]
type Sudoku = [Vector]

main = do
        let list = []
        handle <- openFile "sudoku.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = singlewords
        let sudokus = stringsToSudokus $ stringToStrings $ removeText list
        let result = solveAll sudokus
        printSudokus $ map (\(Just x) -> x) (result)
        --print $ eulerAnswer $ solveAll sudokus
        hClose handle

------------------------------------------------------------------------------------------------------------------------
--                                                 PARSING FILE                                                       --
------------------------------------------------------------------------------------------------------------------------

removeText :: [String] -> [String]
removeText l = filter (\x -> length x > 5) l

stringToStrings :: [String] -> [[String]]
stringToStrings l = aux l [] where
  aux [] acc = acc
  aux li acc = aux (drop 9 li) (acc ++ [take 9 li])

stringsToSudokus :: [[String]] -> [Sudoku]
stringsToSudokus l = map (\x -> dataToSudoku $ stringToInt x) l

------------------------------------------------------------------------------------------------------------------------
--                                                DATA TO SUDOKU                                                      --
------------------------------------------------------------------------------------------------------------------------

stringToInt :: [String] -> [Line]
stringToInt s = map (\x -> map (digitToInt) x) s

dataToLine :: [Line] -> [[Line]]
dataToLine m = map (\l -> [take 3 l,drop 3 $ take 6 l, drop 6 $ take 9 l]) m --not very nice

dataToSquare :: [Line] -> Vector
dataToSquare m = map (\(l,c) -> map (\ligne -> ligne !! c ) $ take 3 $ drop (3*l) $ dataToLine m) [(i,j) | i <- [0..2], j <- [0..2]]

dataToSudoku :: [Line] -> Sudoku
dataToSudoku m =  map (\(l) -> take 3 $ drop (3*l) $ dataToSquare m) [i | i <- [0..2]]

------------------------------------------------------------------------------------------------------------------------
--                                                     SUDOKU                                                         --
------------------------------------------------------------------------------------------------------------------------

transposeSudoku :: Sudoku -> Sudoku
transposeSudoku s = map (\i -> getColumn i s) [i | i <- [0..2]]

getColumn :: Int -> Sudoku -> Vector
getColumn c s = [(getSquare 0 c s), (getSquare 1 c s), (getSquare 2 c s)]

getSquare :: Int -> Int -> Sudoku -> Square
getSquare l c s = (s !! l) !! c

getVector :: Int -> Sudoku -> Vector
getVector l s = s !! l

getLineFromSquare :: Int -> Square -> Line
getLineFromSquare l s = (s !! l)

getColumnFromSquare :: Int -> Square -> Column
getColumnFromSquare c s = getLineFromSquare c (transpose s)

getElementFromSquare :: Int -> Int -> Square -> Int
getElementFromSquare l c s = (s !! l) !! c

getLineFromSudoku :: Int -> Sudoku -> Line
getLineFromSudoku l s = concat (map (\sq -> getLineFromSquare (l `mod` 3) sq) (getVector (l `quot` 3) s))

getColumnFromSudoku :: Int -> Sudoku -> Line
getColumnFromSudoku c s = concat (map (\sq -> getColumnFromSquare (c `mod` 3) sq) (getVector (c `quot` 3) (transposeSudoku(s))))

getElementFromSudoku :: Int -> Int -> Sudoku -> Int
getElementFromSudoku x y s = getElementFromSquare (x `mod` 3) (y `mod` 3) (getSquare(x `quot` 3) (y `quot` 3) s)

------------------------------------------------------------------------------------------------------------------------
--                                                   ALGO HELPER                                                      --
------------------------------------------------------------------------------------------------------------------------

possibilitesInSquare :: Square -> [Int]
possibilitesInSquare s = ([i | i <- [1..9]]) \\ (filter (>0) (concat(s)))

possibilitesInLine :: Line -> [Int]
possibilitesInLine l = ([i | i <- [1..9]]) \\ (filter (>0) l)

possibilitesInColumn :: Column -> [Int]
possibilitesInColumn c = ([i | i <- [1..9]]) \\ (filter (>0) c)

possibilites :: Int -> Int -> Sudoku -> [Int]
possibilites l c s = intersect (possibilitesInSquare (getSquare (l `quot` 3) (c `quot` 3) s)) (intersect (possibilitesInColumn (getColumnFromSudoku c s)) (possibilitesInLine (getLineFromSudoku l s)))

searchMinPossibilites :: Sudoku -> (Int,Int,[Int])
searchMinPossibilites s = aux s 0 0 (-1) (-1) [1,2,3,4,5,6,7,8,9,0] where
  aux s 8 8 resx resy p = if (getElementFromSudoku 8 8 s) == 0
      then if (length(possibilites 8 8 s) < length(p)) then (8,8,(possibilites 8 8 s)) else (resx,resy,p)
      else (resx,resy,p)
  aux s x 8 resx resy p = if (getElementFromSudoku x 8 s) == 0
      then if (length(possibilites x 8 s) < length(p)) then aux s (x+1) 0 x 8 (possibilites x 8 s) else aux s (x+1) 0 resx resy p
      else aux s (x+1) 0 resx resy p
  aux s x y resx resy p = if (getElementFromSudoku x y s) == 0
      then if (length(possibilites x y s) < length(p)) then aux s x (y+1) x y (possibilites x y s) else aux s x (y+1) resx resy p
      else aux s x (y+1) resx resy p

replace :: a -> Int -> [a] -> [a]
replace newVal pos list = take pos list ++ newVal : drop (pos+1) list

playSudoku :: Int -> Int -> Int -> Sudoku -> Sudoku
playSudoku v x y s = replace (replace (replace (replace v (y `mod` 3) (getLineFromSquare (x `mod` 3) (getSquare (x `quot` 3) (y `quot` 3) s)) ) (x `mod` 3) (getSquare (x `quot` 3) (y `quot` 3) s)) (y `quot` 3) (getVector (x `quot` 3) s)) (x `quot` 3) s
--Woooow 231-length oneliner ... Here we "recursively" replace an element in the sudoku. From the sudoku we replace the vector, in the vector we replace the square, in the square we replace the line and in the line we replace the element (y)

isComplete :: Sudoku -> Maybe Sudoku
isComplete s = if ((foldr (+) 0 (concat (concat (concat s)))) == 405 ) then Just s else Nothing

solveSudoku :: Sudoku -> Int -> Int -> [Int] -> Maybe Sudoku
solveSudoku s (-1) (-1) _ = Just s
solveSudoku s x y (p:[]) = let (xx,yy,pos) = (searchMinPossibilites (playSudoku p x y s)) in
  let res = (solveSudoku (playSudoku p x y s) xx yy pos) in
  if res == Nothing then Nothing else res
solveSudoku s x y (p:ps) = let (xx,yy,pos) = (searchMinPossibilites (playSudoku p x y s)) in
  let res = (solveSudoku (playSudoku p x y s) xx yy pos) in
  if res == Nothing
    then let (xxx,yyy,poss) = (searchMinPossibilites (playSudoku (head ps) x y s)) in solveSudoku (playSudoku (head ps) x y s) xxx yyy poss
    else res
solveSudoku s x y [] = let (xx,yy,p) = (searchMinPossibilites s) in Nothing

------------------------------------------------------------------------------------------------------------------------
--                                                   Entry Point                                                      --
------------------------------------------------------------------------------------------------------------------------

solve :: Sudoku -> Maybe Sudoku
solve s = let (xx,yy,pos) = (searchMinPossibilites s) in solveSudoku s xx yy pos

solveAll :: [Sudoku] -> [Maybe Sudoku]
solveAll ss = map solve ss

eulerAnswer :: [Maybe Sudoku] -> Int
eulerAnswer s = foldr (+) 0 $ map (\[c,d,u] -> c*100+d*10+u) (map (\y -> getLineFromSquare 0 y) (map (\x -> getSquare 0 0 x) (map (\(Just x) -> x) (s))))

verifyAllSuokus :: [Maybe Sudoku] -> Int
verifyAllSuokus ss = foldr (*) 1 (map (\s -> if isJust s then 1 else 0 ) ss)

------------------------------------------------------------------------------------------------------------------------
--                                                Printing a Sudoku                                                   --
------------------------------------------------------------------------------------------------------------------------

printSudokus :: [Sudoku] -> IO()
printSudokus ss = mapM_ printSudoku ss

printSudoku :: Sudoku -> IO()
printSudoku s = mapM_ (\i -> if i > 8 then print "-----------------" else print (getLineFromSudoku i s)) [i | i<-[0..9]]
