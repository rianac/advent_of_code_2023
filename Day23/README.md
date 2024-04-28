## Day 23

Nice problem from an algorithmic point of view. After fast transforming input grid into an array, I started to think of algorithms.

First, out of curiosity, I implemented a bruteforce solution, but without much hope for success. My solution used depth-first search to find all paths from the start position to the goal position combined with branching at junction positions (positions with more than two neighbourhing positions). My intuition was right - the solution worked for the first task, but not for the second one (not surprisingly - the longest path is NP hard in general).

Next, I tried to find another useful algorithm. I found the algorithm for the longest path in DAG (Directed Acyclic Graph). The algorithm is based on topological sorting of graph nodes in such way, that a node must be located after all those nodes, from which graph edges go to the given node. Since the given input had special characteristics, like

- there were only a few junction positions, where a path can continue following more branches,
- every path from the start position to the end position was mainly composed of straight lines,
- slopes limited all segments from one junction position to another one and made them passable only in one direction,

it was possible to transform the grid into DAG with nodes represented by junction positions (plus start and end positions). The code had no problem with the first task. But ignoring slopes in the second task resulted in inability to transform the grid into DAG making my second attempt unusable for this task.

Therefore, in order to solve the second task, I transformed the grid into a directed cyclic graph and used the previous approach - depth-first search. Since the number of graph nodes was considerably smaller then the number of grid positions, this "compressed representation" did not represent any problem.

## What I have learnt:

- an algorithm for longest path in DAG
