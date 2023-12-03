import Control.Monad
import System.IO

main = do
  txt <- readFile "input.txt"
  let content = lines txt
  let calibrations = map processCalibration content
  print (sum calibrations)

processCalibration :: String -> Int
processCalibration cal =
  case digits of
    [] -> 0
    [x] -> read [x, x]
    xs -> read [head xs, last xs]
  where
    digits = trimCalibration cal

trimCalibration :: String -> String
trimCalibration = filter (\x -> x `elem` ['0' .. '9'])
