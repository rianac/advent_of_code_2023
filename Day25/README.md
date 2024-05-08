## Day 25

Again the problem with a special structure of problem input, providing lower complexity than a general form. The input graph consisted of two highly-connected subgraphs, which were connected by three edges only. Moreover, the two subgraphs had approximately the same number of nodes. Following reddit discussions, I decided to give chance to brute force approach (to generate paths between pairs of nodes, count edge frequencies, and take three most frequent edges as three cuts).

Parsing the graph into a map of nodes and their neighbours was quite easy. Next, an exercise in coding graph search - breadth-first search in this case. An interesting part was how to select node pairs (start-goal node pairs) to fire the search. Since the overall number of node pairs was too big, random selection came onto the scene. I must admit, that using random number generators in Haskell is not my favourite part of the language. 

The used approach (stochastic brute-force) represented heuristics and not algorithm - sometimes the resulting code was able to partition the graph into two subgraphs but sometimes was not able.

## What I have learnt:

- basic working with randomly generated numbers
- sorting a list of elements randomly
