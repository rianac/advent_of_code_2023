## Day 17

One more algorithmically demanding day. I decided to use classical state space search - uniform cost search. Following my Lisp heritage, I selected the version of the algorithm based on the existence of two lists (*open* and *close*). First, I coded the algorithm using Haskell lists to represent both *open* and *close*. Next, in order to make the program a bit faster, I replaced the list implementation of *close* (used to accumulate and compare visited states) with Haskell set implementation.

As a result, my code stopped to produce right results. I had a deep experience of hunting for a bug in a situation, when I had no clue what was going on. The problem was how I solved one of algorithm's features - it sorts nodes according to value of cost function, while this value has no role in comparison of nodes whether they are equal or not. In addition, the comparison of elements based on lists (`elem`) requires nodes as instance of Eq typeclass while comparing based on sets (`member`) requires nodes as instance of Ord typeclass. I learnt in a hard way that this was a source of different program behaviour patterns.

Nevertheless, my program was not a great success. It calculated both tasks, but the first calculation took almost one minute while the second task required around ten minutes. It seems, I selected not the best approach how to solve the task.

Edit: I prepared a more optimized version of the code with including these changes

- only valid positions are generated instead of generating all possible positions with subsequent filtering only the valid ones,
- moves of different sizes in the straight direction are allowed instead of moving by one step only,
- *open* is represented as a set instead of a list with no need of permanent explicit sorting.

As a result, runtime decreased to around one second.

## What I have learnt:

- more intense than usual usage of record-like datatypes
- importance of required typeclass instances on which used functions are based
- new functions operating on sets (`findMin`, `deleteMin`, `deleteFindMin`)
