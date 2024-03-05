## Day 10

A grid-like input again. Based on the first task, I decided not to represent the grid tiles but only transitions which do exist in the grid. And to forget about tiles not participating in any transition. First, I generated a cyclic path using good old fashioned recursion, but later I rewrote the function in non-recursive manner using `unfoldr` function.

The second task was about tiles within the path - including also those I removed from my representation. I was lazy to change my representation to another one better suited to the task, instead I tried to find an algorithm for which representation of cyclic path is enough. After looong time, I finally came with the algorithm and completed the task. 

## What I have learnt:

- new functions (`init`, `uncurry`, `unfoldr`)
