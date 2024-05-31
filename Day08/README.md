## Day 8

Given a graph of nodes where each node is connected with two other nodes, the task reduces to finding the length of a path in the graph while following a given sequence of steps. Easy parsing into simple data structures (I used only tuples and lists) using a few lines of code (no need to use any parser combinators library).

The first task (one path) was easy - simple graph traversal. The second task (several coordinated paths) made no difficulties while extending the code used for the first task. Unfortunately, the code was not usable for processing the given input data - it just swallowed all memory of my computer and continued at snail's pace. Reddit helped me in better task understanding - each path was cyclic (can be repeatedly iterated) and considering the given step sequence, the paths were independent. As a result, I found all paths independently and just find LSM (least common multiple) of their lengths.

Thanks to Reddit I discovered function `scanl` enabling to get rid of explicit recursion and a counter to count performed iterations. Since using combo `scanl`+ laziness of expression evaluation seems to me way more haskellish, I prepared the second version of the code.

## What I have learnt:

- Haskell's sensitivity for algorithm selection
- new functions (`scanl`)
