# Assignment 2

### Submission by
* Abhinav Singh 140101002
* Yash Pote 140101080

### Finding Relationship and gender
uncle and halfsister functors are implemented in 2.1

It is expected that the parent and gender relations are known at runtime either through hardcoding or dynamic assignment.

### Searching for details of a research scholar
search functor for both name and roll is implemented in 2.2
It searches for relevant info and displays it in a table.

We assume that the list type 1 are named as first and list type 2 are named as second.
It is expectedd that the lists of both types are consistent and known at runtime.

### Path finding problem

path and min_path are the two public functors used for part A and for part B respectively.
We have implemented a very inefficient version of BFS. But it works and let's us do path(X,Y,L,P) or min_path(X,Y,L,P) ie. have all the arguments as variables. Usually part A should be implemented using DFS and B using BFS, but ours looks waaaay cooler. 

It is assumed that we know at runtime:
* total number of columns and rows
* up, down, left, right walls
* start and end points

For example if there is a wall at the right of 2 position then we add 2 to walls_right rule.

We have used the naming scheme for the positions as given in the pdf. For example at position 12, our position is 12 and not [2,6].
