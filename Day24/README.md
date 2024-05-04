## Day 24

The problem required to brush up basic geometry knowledge. To be frank, I spent way more time to derive equations on paper than to write code.

Having derived necessary equations, the selected approach was easily transferable into code. I faced only one issue - input provided integer values as well as results were expected to be also integer values, but calculation required also

- usage of (not integer) division,
- construction of inverse matrix.

In order to meet these requirements, I had to convert integer values into fractional values. Since I had also to test calculated fractional values, whether they are equal or not, I decided not to use neither floats nor doubles. My selection was to manipulate with rational numbers.

## What I have learnt:

- basic work with Either
- some new functions from Data.Maybe (`isJust`, `fromJust`, `maybeToList`)
- numerical pyramid, transformations to/from Rational
- basic operations from Data.Matrix (`fromList`, `fromLists`, `toList`, `inverse`, `multStd`)
