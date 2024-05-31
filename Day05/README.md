## Day 5

Although the input format was a bit more complicated,  parser combinators ReadP served me quite well.

Task was relatively easy, a simple algorithm based on pushing each input number through a pipe of mappings was enough. At least for the first task. Trying to solve the second task I hit the wall - my program took almost two hours to complete. Well, scalability could be a beast.

Therefore (based on ideas of experienced haskellers), I changed the algorithm - instead of working with numbers I rewritten it to work with intervals of numbers. It works like a charm. Both algorithms are here in different files.

## What I have learnt:

- basic work with Data.Map
- comparison of intervals
- knowledge that the best remedy for scalability problem is to find more efficient algorithm


