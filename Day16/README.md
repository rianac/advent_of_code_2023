## Day 16

The day was a bit demanding algorithmically. To follow a beam was not so easy in conditions where it could (and really did)

- run out of the grid and die
- fork itself into two separate beams running in opposite directions
- return at previously seen position and run in a loop

as well as to combine all behaviour types together. But after sorting a proper algorithm, its coding was almost straightforward.

The first task was easy, the second task basically involved several repetitions of the first task within different initial conditions. My first attempt needed more than eleven minutes to complete the tasks. My intuition was that my problem is in determining repeatedly whether an element is present in a long list. Replacing querying long lists of elements with querying sets of elements or with testing element values directly helped to shorten runtime to five seconds.

## What I have learnt:

- querying long lists is possible but not a good idea
- creating sets (`singleton`, `fromList`)
- basic work with sets (`member`, `notMember`, `map`, `union`)

