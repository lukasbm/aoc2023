-- calculate race dist
raceDist :: Int -> Int -> Int
raceDist totalTime accTime = (totalTime - accTime) * accTime

waysToBeatRecord :: (Int, Int) -> Int
waysToBeatRecord (time, record) = foldl go 0 [1 .. time]
  where
    go :: Int -> Int -> Int
    go acc x = if raceDist time x > record then acc + 1 else acc

main = do
  txt <- readFile "input.txt"
  let times :: [Int] = map (read :: String -> Int) $ words $ drop 10 $ (lines txt !! 0)
  let records :: [Int] = map (read :: String -> Int) $ words $ drop 10 $ (lines txt !! 1)

  let ways = map waysToBeatRecord $ zip times records

  print $ ways

  print $ product ways
