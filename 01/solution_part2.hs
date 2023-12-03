import Data.Char (isDigit)
import qualified Data.Text as T

main = do
  txt <- readFile "test_part2.txt"
  let content = lines txt
  -- let content = ["asdfour5", "", "3"]
  let calibrations = map (calcCalibration . processCalibration) content
  print calibrations
  print (sum calibrations)

processCalibration :: String -> String
processCalibration "" = ""
processCalibration cal = filter isDigit (head calReplaced : processCalibration (tail calReplaced))
  where
    calReplaced = replaceHuman cal --  filter isDigit (replaceHuman cal)

calcCalibration :: String -> Int
calcCalibration digits = do
  case digits of
    [] -> 0
    [x] -> read [x, x]
    xs -> read [head xs, last xs]

replaceHuman :: [Char] -> [Char]
replaceHuman cal = case cal of
  'o' : 'n' : 'e' : rest -> '1' : rest
  't' : 'w' : 'o' : rest -> '2' : rest
  't' : 'h' : 'r' : 'e' : 'e' : rest -> '3' : rest
  'f' : 'o' : 'u' : 'r' : rest -> '4' : rest
  'f' : 'i' : 'v' : 'e' : rest -> '5' : rest
  's' : 'i' : 'x' : rest -> '6' : rest
  's' : 'e' : 'v' : 'e' : 'n' : rest -> '7' : rest
  'e' : 'i' : 'g' : 'h' : 't' : rest -> '8' : rest
  'n' : 'i' : 'n' : 'e' : rest -> '9' : rest
  x -> x

-- '0' : rest -> '0' : rest
-- '1' : rest -> '1' : rest
-- '2' : rest -> '2' : rest
-- '3' : rest -> '3' : rest
-- '4' : rest -> '4' : rest
-- '5' : rest -> '5' : rest
-- '6' : rest -> '6' : rest
-- '7' : rest -> '7' : rest
-- '8' : rest -> '8' : rest
-- '9' : rest -> '9' : rest
-- x : xs -> xs
-- [] -> []
