# Day 3

Have to analyze this space around each number
```
******
*1234*
******
```

Q: do other numbers count as symbols?
A: i don't think so

Q: do the numbers wrap to next line?
A: No

Note: numbers can share symbols

This might be the first time I have to use a 2D array
https://www.reddit.com/r/haskell/comments/loj3x7/2dimensional_algebraic_data_type/

solution could be a backtracking algorithm that walks into every direction.
start it from every digit in the input
will find duplicates, but can just go distinct + sum
or use backtracking to get the right sub-rectangle
sub rectangle would make sense, also because of behaviour on the edges
just need to know the number and then check if symbol in there


More discussion
https://www.reddit.com/r/haskell/comments/189m9oi/advent_of_code_2023_day_3/

