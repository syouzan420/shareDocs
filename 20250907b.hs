import Control.Monad (replicateM)

type Pos = (Int,Int)

main :: IO ()
main = do
  [h,w] <- getLine >>= return . (map read) .words
  ss <- replicateM h $ getLine
  let bl = check (h,w) (1,1) ss
  let res = if bl then "Yes" else "No"
  putStrLn res


pairToBool :: Pos -> [String] -> Bool
pairToBool (y,x) strs =
  let gyou = strs !! (y-1)
      c = gyou !! (x-1) 
   in c == '#'

neighbour :: Pos -> Pos -> [Pos]
neighbour (h,w) (a,b)
  = filter (\(y,x) -> x>=1 && x<=w && y>=1 && y<= h) [(a-1,b),(a+1,b),(a,b-1),(a,b+1)]


nextPos :: Int -> Pos -> Pos 
nextPos w (y,x)
  | x==w = (y+1,1)
  | otherwise = (y,x+1)

checkNei :: Pos -> Pos -> [String] -> Bool
checkNei hw ps strs =
  let nei = neighbour hw ps
      bls = map (\pos -> pairToBool pos strs) nei
   in and bls 

check :: Pos -> Pos -> [String] -> Bool
check (h,w) pos strs =
  if fst pos > h then True else 
    let isB = pairToBool pos strs
    in if isB then (checkNei (h,w) pos strs) && (check (h,w) (nextPos w pos) strs) else check (h,w) (nextPos w pos) strs
  
