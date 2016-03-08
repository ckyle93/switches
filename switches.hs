import System.IO

-- Makes the list of switches, with a default value of False (off). 
makeLights :: Int -> [Bool]
makeLights x = [False | _ <- [1..x]]

-- Toggles the x'th element in the switch list.
-- See http://stackoverflow.com/questions/5852722/ \
-- replace-individual-list-elements-in-haskell
-- for the answer given by Don Stewart, this code is adapted from his
--answer.
toggle :: Int -> [Bool] -> [Bool]
toggle x lst
       -- let (y,z:ys) = splitAt x lst
        | z == True = y ++ False : ys
        | otherwise =  y ++ True : ys
        where (y,z:ys) = splitAt x lst
        

-- Toggles the lights from low to high.
-- Toggle lights must be called with the first Int in the tuple being
-- less than or equal to the second.
toggleLights :: (Int, Int) -> [Bool] -> [Bool]
toggleLights (low, high) lst
    | low > high = error "bad indexes"
    | low == high = toggle low lst 
    | low < high = toggleLights (low + 1, high) (toggle low lst)

orderTuple :: Int -> Int -> (Int, Int)
orderTuple x y
            | x < y = (x, y)
            | otherwise = (y, x)

-- Toggles the ranges of lights given in the first argument
checkLights :: [String] -> [Bool] -> [Bool]
checkLights [] switches = switches
checkLights (x:xs) switches =
    checkLights xs (toggleLights range switches)
    where y = words x
          range =  orderTuple (read (head y) :: Int) (read (last y) :: Int)

countLights :: [Bool] -> Int
countLights lst = length (filter (==True) lst) 

main = do
    contents <- readFile "test.txt"
    let lst = lines contents
        lights = makeLights (read (head (lst)) :: Int)
    putStr (show (countLights (checkLights (tail lst) lights))++ "\n")
