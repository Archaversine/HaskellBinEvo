# Haskell Binary String Evolution

See writeup [here](https://medium.com/knoxcs/binary-string-evolution-in-haskell-574afbbcdc59).

## Problem

Evolve the largest number you can represent with a 20-bit string.

## Implementation

### 20 Bit Strings

Instead of storing an array of 20 1s and 0s, the `Word32` type from `Data.Word` 
can be used to store the relevant data. All calculations with the `Word32` type
will be made such that the other 12 bits will always be 0. This allows for the
bitcount of the `Word32` to be the fitness function itself and allows for the use
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
crossed, and then mutated which yields the child for the next generation.

## Results

### Visualization

Using the matplotlib bindings for Haskell, a simple visualization can be
generated that shows how different parameters affect the efficiency of the
genetic algorithm.

Here are the details for each experiement:

|  | Mutation Rate | Crossover Rate | Crossover Type | Elitism | Population Size |
|---|---|---|---|---|---|
| EXP 0 | 0% | 100% | Single Point | Top 25 | 50 |
| EXP 1 | 100% | 80% | Single Point | Top 1 | 50 |
| EXP 2 | 1% | 20% | Single Point | None | 50 |
| EXP 3 | 50% | 80% | Single Point | Top 1 | 50 |
| EXP 4 | 10% | 100% | Single Point | 0 | 100 |

And here is a graph of the results showing how long it took each strategy to
reach the optimal solution (all 1s):

![Graph](https://raw.githubusercontent.com/Archaversine/HaskellBinEvo/main/visualization.svg) 

### Results from an Efficient Strategy

Sample results from running the program:

```
--------------------------------------------
Population Size: 500
Number of Generations: 30
Number of Iterations: 30
Mutation per bit: 0.1%
--------------------------------------------
Iteration 1: 11111111111111111111 = 1048575
Iteration 2: 11111111111111111111 = 1048575
Iteration 3: 11111111111111111111 = 1048575
Iteration 4: 11111111111111111111 = 1048575
Iteration 5: 11111111111111111111 = 1048575
Iteration 6: 11111111111111111111 = 1048575
Iteration 7: 11111111111111111111 = 1048575
Iteration 8: 11111111111111111111 = 1048575
Iteration 9: 11111111111111111111 = 1048575
Iteration 10: 11111111111111111111 = 1048575
Iteration 11: 11111111111111111111 = 1048575
Iteration 12: 11111111111111111111 = 1048575
Iteration 13: 11111111111111111111 = 1048575
Iteration 14: 11111111111111111111 = 1048575
Iteration 15: 11111111111111111111 = 1048575
Iteration 16: 11111111111111111111 = 1048575
Iteration 17: 11111111111111111111 = 1048575
Iteration 18: 11111111111111111111 = 1048575
Iteration 19: 11111111111111111111 = 1048575
Iteration 20: 11111111111111111111 = 1048575
Iteration 21: 11111111111111111111 = 1048575
Iteration 22: 11111111111111111111 = 1048575
Iteration 23: 11111111111111111111 = 1048575
Iteration 24: 11111111111111111111 = 1048575
Iteration 25: 11111111111111111111 = 1048575
Iteration 26: 11111111111111111111 = 1048575
Iteration 27: 11111111111111111111 = 1048575
Iteration 28: 11111111111111111111 = 1048575
Iteration 29: 11111111111111111111 = 1048575
Iteration 30: 11111111111111111111 = 1048575
--------------------------------------------
Minimum Fitness: 20.0
Maximum Fitness: 20.0
Average Fitness: 20.0
Standard Deviation: 0.0
--------------------------------------------
```

