## Day 20

Input files provided relatively rich input format. Thus, I used `ReadP` parser combination library to parse input files. Parsed data were transformed into a map of nodes - this captured static view on a graph of modules. Next, another map was added to represent dynamic view on the module network. Thanks to used library, there was no problem in building and filling data structures I defined.

The first task was basically a simulation - simulation of individual node behaviour as well as simulation of the node network as a whole. The core of my code simulate changes of node states, memories of node inputs and management of a queue of messages (pulses) transferred between nodes. 

In order to adjust the code to the second problem, I wrote only a modified version of one of the previously developed functions. The result was as usual - the approach, successful in the first task, was unusable in the second one. 

In this situation, I tried to find whether today's network is a general one or it is a specially crafted one which could be solved in a more algorithmic way instead of using lengthy simulation. Inspection of the network revealed, that in order for a special output node to receive a particular pulse, it is necessary for four other nodes to receive this particular pulse. My next hypothesis was, that the network is basically consisting of four networks - each one supplying one of the four nodes. And that processes in these subnetworks are periodic - while these subnetworks have different periods. To prove this hypothesis, I found periods for the subnetworks and calculated their least common multiple.

## What I have learnt:

- heavy usage of (`data` and `type`) user defined data types to represent structures and bring in problem specific lingo
- using structures to manage queue like operations in FIFO manner
