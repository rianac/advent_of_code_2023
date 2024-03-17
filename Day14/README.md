## Day 14

Rolling oval stones to one end of a line is equivalent to rolling free places to the other end. It sounds like sorting with proper ordering of oval stones and free places. But cube stones deny rolling - they are immune to sorting. Simple solution - to cut line into smaller lines without cube stones, sort them separately and finally restore the original line from small ordered line segments and cube stones in their original positions. This idea of replacing rolling only some objects with sorting together with careful rotating the whole grid anticlockwise made me a day - task1 done.

The second part required to rotate the grid by a really big number of times. This ridiculous number of rotations stopped me, since it was impenetrable by my program. Only one hope - maybe the problem is not as difficult as it seemed. After an analysis of a few tens of rotations, I discovered a pattern - periodical repeating grid states with a fixed period length. After discovery, when this periodic process begins, it was possible to calculate in which state the grid will be after any number of rotations. I just found this number of rotations, made a short simulation within the periodic pattern and calculated the final state.

That was a real fun.

## What I have learnt:

- new functions in Data.List (`transpose`)
