

main :: IO ()
main = do
  s <- getLine
  let ab = getNum s
  let (na,nb) = nextStage ab
  let res = show na ++ "-" ++ show nb
  putStrLn res

getNum :: String -> (Int,Int)
getNum [a,_,b] = (read [a], read [b])
getNum _ = (0,0)

nextStage :: (Int,Int) -> (Int,Int)

nextStage (a,b)
  | a == 8 = (0,0)
  | b == 8 = (a+1,1)
  | otherwise = (a,b+1)
