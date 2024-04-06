## Day 18

Another problem to calculate the area defined by a circular path on a rectangular grid. In order not to repeat the approach used previously, I started with brute force *flood* algorithm. On a rectangular grid I
- marked all those positions from which the circular path consists
- marked all positions *outside* of the path - recursively flooding from all edges towards the path
- calculated the size of the grid minus the number of outside positions.
It worked for the first task without any problem.

The second task was basically the first one on steroids. Ridiculously huge upscaling prevented usage of my brute force approach. Thus, I tried another approach not constructing an explicit grid. I wanted to construct a path as a sequence of positions and subsequently calculate the length of horizontal cuts. Unfortunately, the attempt to produce the list of positions representing the path was not successful due exhausting the available computer memory.

I was able to construct the path as a list of vertexes - positions at the beginning and at the end of linear path segments. Based on hint from the Internet I studied the Shoelace algorithm. Implementation was easy and the produced code solved the problem under one second.

## What I have learnt:

- Shoelace formula enabling to calculate the area of a simple polygon
- new functions (`readHex`, `quot`, `scanl`)
