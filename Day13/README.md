## Day 13

In order to be able to compare rows each to other as well as columns, I decided to represent input grid redundantly twofold - as a list of rows as well as a list of columns. Parsing input into a list of rows was quite easy by a two-liner. Then I transformed it into a list of columns - I created a matrix from lines, transposed the matrix (changed rows and columns) and transformed it into a list of lists back.

The algorithm was based on stating, which rows/columns should be identical to represent reflexion. First, I prepared lists of corresponding index pairs. Subsequent reflection check of different reflection lines was easy - just strictly following prepared index pairs.

In the second part I realized, that there is no need to find the exact positions of smudges - only to find those reflexion candidates which were hampered by exactly one smudge somewhere on the grid.

## What I have learnt:

- existence of module Data.Matrix and transferring data between matrices and lists of lists
- a few extras making dealing with tuples a bit easier (`&&&`, `first`, `second`)
- it is not the easiest task to hunt for a bug (signalized by a cryptic error message on difference between expected and actual types) in a function with many other functions in its `where` part
