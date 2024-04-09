## Day 19

After longer time a task not defined on a grid. Input data were a bit more complicated. Therefore, I used a parser combinator library to parse the data and established proper data type structures - two maps (one for workflows and one for representing each part) and a few lists and tuples. After remembering how to use the library, I had no problem in this part.

The first part was not difficult - just to process a list of parts, while each part traversed through a bunch of tests in order to be finally accepted or rejected.

The second part required a change in used approach, since I rejected the brute force approach based on checking each possible part. A map of four `(low, high)` ranges was defined at the root of a binary tree, nodes of which were represented by rules of the given workflows - the condition of the rule in each node could be satisfied (the left branch) or unsatisfied (the right branch). The map of ranges traversed the branches of the tree and modified the ranges appropriately according to branches.


## What I have learnt:

- representing data as partial functions is a very elegant approach but very unfriendly for debugging (since functions cannot be printed in repl)
- new functions (`concatMap`, `product`)
