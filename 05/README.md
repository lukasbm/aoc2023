
```hs
-- readP tutorial: https://web.ecs.syr.edu/courses/cis352/slides/19parsing34up.pdf

parse :: ReadP a -> String -> [(a, String)]
parse = readP_to_S

parseWith :: ReadP a -> String -> a
parseWith p s = case [a | (a, t) <- parse p s, all isSpace t] of
  [a] -> a
  [] -> error "no parse"
  _ -> error "ambiguous parse"
```
