# InterleavedBinaries

Combines two binaries into one, by taking the instructions of one and interleaving them with the other. The instruction pointer goes from one command to the next as it normally would, alternating work between the two binaries.

## Pros:
1) Time not spent on pesky context switching between processes
2) Convenient to only run one binary instead of two - half the commands needed!

## Cons:
Too many to count, but if we try:
1) The programs will overwrite each other's registers.
2) How to share the stack?
3) System calls require specific registers, so we can't mess with the registers used in this case.
4) How would branching even work? We only have one instruction pointer!
5) How would looping work?
6) What about special purpose registers that can't be shuffled around?
7) Even if we fix all of these issues, won't the awful hacks mean it won't run any faster?

