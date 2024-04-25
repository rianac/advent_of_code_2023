## Day 22

The problems were not very demanding. I took coding as a relaxing activity, focusing more on how the code looks than on algorithmic side. 

First, I represented blocks as lists of cubes and let them slowly fall until they settled on ground or other blocks. The fall was really slow - iterative fall of blocks without support by one vertical step until everything settled down. Next I investigated positions of the blocks and represented which block was supported by which blocks. 

The first task was based on finding those blocks, which did not support anything or supported other blocks in combination with some other block.

The first task was based on finding those blocks, which supported other blocks without sharing this support with any other block. And again, iterative search through the stack of blocks.

## What I have learnt:

- a few nice hints given by hlint
