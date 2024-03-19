## Day 15

The first task was very easy, almost no parsing followed by almost no computation. Easy.

The important part of the second task was how to select a proper representation for a number of boxes, each containing a number of lenses, satisfying:
- individual boxes should be accessible, but also their positions in ordered sequence are important
- ordering of lenses is important and this ordering should allow special type of changes (removing from the ordering, adding at the end)
- modifications of several lenses at the same time.

I decided to use `Map Int [(String,Int)]` where boxes use integer indexes and each box is a list of lenses, while each lens has label and value. It proved as a quite flexible representation, allowing all required operations.

## What I have learnt:

- new functions for working with ASCII codes (`fromEnum`, `toEnum`, `ord`)
- one more way how to split lists (`splitOneOf`)
