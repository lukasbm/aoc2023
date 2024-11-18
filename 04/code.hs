import Data.Text.Split (splitOn)


splitOnChar :: Char -> String -> (String, String)
splitOnChar c s = (takeWhile (/= c) s, dropWhile (== c) s) 


wordsWhen

main = do
    txt <- readFile "test_part1.txt"
    let content = map (snd . split ":") (lines txt)
    -- remove prefix
    print content [0]

-- get list of winning numbers i have
-- point score is 2^(len of winning numbers i have)
-- len = 0 --> 0 points


