## Day 21

The problem was easy and not so easy. I parsed the input grid into an array. And simulated stepping through the grid trying to avoid stones. The first task was done - this was the easy part.

The dimensionality of the second part excluded simulation as a viable approach. Thus, time to search for a particular feature making the input different (read *easier*) from a general case. And here it was - the number of steps enabled to stop exactly at the far edge of a copy of the grid. I have found a sketch of a geometrical approach to solve such problem and I decided to follow it. Based on it, I implemented the algorithm, but my results were rejected. I was frustrated, since I was not able to find where the problem in my implementation is. Probably, day 21 will be the first task I am not able to finish. This was the not so easy part.

Edit: After a few weeks later, I returned to the problem. And I was lucky to find why I was not successful previously. I simply forgot about the existence of such positions, which are totally inaccessible, since they are surrounded by stones from all sides. Silly overlooking - sometimes it happens.

## What I have learnt:

- to be patient when things do not go as expected
