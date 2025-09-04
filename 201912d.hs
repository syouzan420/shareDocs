import Control.Monad (replicateM)

main :: IO ()
main = do
  n <- getLine >>= return .(\i -> (read i) :: Int)
  as <- replicateM n $ getLine >>= return . (\i -> read i :: Int)
  let cs = [1..n]
  let y = tyoufuku n as
  let x = nuketeru as cs
  let res = if x==0 then "correct" else show y ++" "++show x
  putStrLn res

nuki :: Int -> [Int] -> [Int]
nuki i lst = take i lst ++ drop (i+1) lst

nuketeru :: [Int] -> [Int] -> Int
nuketeru _ [] = 0
nuketeru as (x:xs) = 
  if x `elem` as then
      nuketeru as xs else x

tyoufuku :: Int -> [Int] -> Int
tyoufuku 0 _ = 0
tyoufuku i xs = let d = i - 1  
                 in if (xs!!d) `elem` (nuki d xs)
    then xs!!d
    else tyoufuku d xs
