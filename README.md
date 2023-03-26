# Haskell Binary String Evolution

## Problem

Evolve the largest number you can represent with a 20-bit string.

## Implementation

### 20 Bit Strings

Instead of storing an array of 20 1s and 0s, the `Word32` type from `Data.Word` 
can be used to store the relevant data. All calculations with the `Word32` type
will be made such that the other 12 bits will always be 0. This allows for the
value of the `Word32` to be the fitness function itself and allows for the use
of bitwise operations to manipulate the 1s and 0s. 

### Crossover

Crossover is implemented using the standard crossover technique, where both
parents are split into two parts and merged to create a child. Thanks to
bitwise operations, this process is not too difficult. Below is the algorithm
used to calculate the child's DNA. Note that this algorithm assumes the largest
12 bits of both parents are 0, which allows for no need to modify the 12 bits
after the crossover. Crossover happens always.

```
splitPoint = random int between 0 and 20 inclusive
mask = ~0 << splitPoint
child = (parent1 & mask) | (parent2 & ~mask)
```

### Mutation

For each bit in the 20 bits of the `Word32`, there is a small chance (0.1%)
that it will be flipped after crossover occurs. The bit is flipped with bitwise
xor. Mutation always happens after crossover occurs.

```
n = current bit # (between 0-20 exclusive)
child ^= (1 << n)
```

### Population Selection & Reproduction

When parents are selected from the population, a roulette selection is used to
bias the selection towards candidates with a higher fitness.

To create the next generation, two parents are selected from the population,
crossed, and then mutated which yeilds the child for the next generation.

## Results

Sample results from running the program:

```
--------------------------------------------
Population Size: 500
Number of Generations: 500
Number of Iterations: 30
Mutation per bit: 0.1%
--------------------------------------------
Iteration 1: 11111111111111011011 = 1048539
Iteration 2: 11111111111111111010 = 1048570
Iteration 3: 11111111111111110001 = 1048561
Iteration 4: 11111111111111111011 = 1048571
Iteration 5: 11111111111111110100 = 1048564
Iteration 6: 11111111111110101000 = 1048488
Iteration 7: 11111111111101111000 = 1048440
Iteration 8: 11111111111110010111 = 1048471
Iteration 9: 11111111111111011110 = 1048542
Iteration 10: 11111111111111111010 = 1048570
Iteration 11: 11111111111111111101 = 1048573
Iteration 12: 11111111111001010111 = 1048151
Iteration 13: 11111111111110010111 = 1048471
Iteration 14: 11111111111110101111 = 1048495
Iteration 15: 11111111111111011011 = 1048539
Iteration 16: 11111111100100010010 = 1046802
Iteration 17: 11111111111110111111 = 1048511
Iteration 18: 11111111101111010100 = 1047508
Iteration 19: 11111111111101111110 = 1048446
Iteration 20: 11111111111111010110 = 1048534
Iteration 21: 11111111111111110001 = 1048561
Iteration 22: 11111111111111110100 = 1048564
Iteration 23: 11111111110101110011 = 1047923
Iteration 24: 11111111111111111111 = 1048575
Iteration 25: 11111111111001011010 = 1048154
Iteration 26: 11111111111011110011 = 1048307
Iteration 27: 11111111111111111110 = 1048574
Iteration 28: 11111111111110100010 = 1048482
Iteration 29: 11111111111011101010 = 1048298
Iteration 30: 11111111111111111000 = 1048568
Average: 1048378.4
Standard Deviation: 371.59454
```

Out of the 30 iterations, only iteration 24 was able to produce the best
possible solution.
