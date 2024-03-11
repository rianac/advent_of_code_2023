## Day 12

The problem reminded me a parsing problem. Therefore, I decided to solve it using Haskell's tools for parsing. First, I tried to use regexes but with no success. I was not able to produce overlapping matches (e.g. both matches "?##" and "##?" for a group of 3 over the pattern "?##?"). Thus, I abandoned this approach.

Next, I tried to use parser combinators. After longer tinkering a bit with how to combine parsers into sequences, I came with two solutions - one based on `<*>` (suitable if parsed values are relevant) and the other based on `*>` (if only number of matches is relevant but not parsed values). Both approaches worked well for the first task. Regarding the second task - there was no problem with the given example. But I hit the scalability wall when processing the given input. My program consumed all computer memory with subsequent swapping.

To solve the second task, the approach based on parser combinators is not viable. Therefore, I changed the approach and used depth-first tree search to prepare another code file. Simple search was a way too slow but simple memoization (only for branching tree nodes) made a real difference.

I learnt on reddit that some guys solved the problem using dynamic programming. It took my attention, since, although I know the algorithm, I did not realize that it can be employed. Thus, I prepared the third file with dynamic programming utilizing two strategies - sweep and in-place strategy.

## What I have learnt:

- use parsing combinators for a task of getting one arrangement and not many (especially not millions alternative arrangements)
- new functions (`!!`, `tails`, `Map.mapWithKey`, `Map.insert`)
- instantiating Ord typeclass to change ordering of keys in map
