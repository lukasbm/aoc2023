import Data.Char (isDigit)
import qualified Data.Text as T

main = do
  txt <- readFile "input.txt"
  let content = lines txt
  let calibrations = map (calcCalibration . processCalibration) content
  print calibrations
  print (sum calibrations)

calcCalibration :: String -> Int
calcCalibration digits = do
  case digits of
    [] -> 0
    [x] -> read [x, x]
    xs -> read [head xs, last xs]

processCalibration :: String -> String
processCalibration "" = ""
processCalibration cal = filter isDigit (head calReplaced : processCalibration (tail calReplaced))
  where
    calReplaced = replaceHuman cal

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
