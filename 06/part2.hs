-- calculate race dist
raceDist :: Int -> Int -> Int
raceDist totalTime accTime = (totalTime - accTime) * accTime

waysToBeatRecord :: (Int, Int) -> Int
waysToBeatRecord (time, record) = foldl go 0 [0 + 14 .. time - 14]
  where
    go :: Int -> Int -> Int
    go acc x = if raceDist time x > record then acc + 1 else acc

main = do
  txt <- readFile "input.txt"
  let noSpaces = filter (/= ' ') txt
  let time :: Int = read $ drop 5 (lines noSpaces !! 0)
  let record :: Int = read $ drop 9 (lines noSpaces !! 1)

  let ways = waysToBeatRecord (time, record)

  print $ ways
