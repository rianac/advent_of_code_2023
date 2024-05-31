## Day 7

The problem was focused on sorting cards. This was the opportunity to play with types and automatic instantiation of the Ord typeclass. I defined two new types - one for cards and one for hands. In order to make more fun, I tried to persuade Haskell to parse string into these types directly.

The second task changed required ordering. In order not to define another type, I extended the Card type a bit. As a result, automatically generated ordering of data constructors corresponds to both required ordering patterns.

## What I have learnt:

- sum data types
- instantiation of Read typeclass
- some interesting functions (group, intersperse, intercalate, fromMaybe)
