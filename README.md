# ulam-spiral
Inspired by this video from The Coding Train (https://www.youtube.com/watch?v=a35KWEjRvc0)

This repository includes the Ulam Spiral, also known as the Prime Spiral 

The Ulam spiral or prime spiral is a graphical depiction of the set of prime numbers, devised by mathematician Stanisław Ulam in 1963 and popularized in Martin Gardner's Mathematical Games column in Scientific American a short time later. It is constructed by writing the positive integers in a square spiral and specially marking the prime numbers.(from wikipedia: https://en.wikipedia.org/wiki/Ulam_spiral)

This will be implemented in various languages and platforms. The original video implement the approach on P5.js, and while is a very good implementation, the code in this repo will try the following:

1. A multi-threaded approach, if possible. 
2. Instead of "walking" over the spiral and generating numbers one by one, the program will calculate the position of the number in a matrix stored in memory.
3. This spiral will be printed in console, and not displayed in any UI. 
4. The size of the matrix to show will be received as a parameter

This is NOT intended to be a benchmarking tool for the languages and platforms here. This is done just for fun and learn more languages with something diferent than running a "Hello World" application. 

## The algorithm
The program has two main parts, the Prime number detection, and the calculation of the position on the spiral. As this program will use background threads, it is necessary to know where in memory to put the result of the prime checker, as the order of arrival of the terms is not determined. 

There are two files included in the root directory, Spiral Generation.md, and Prime Numbers.md, which explain the algorithm in detail, if implementation details is needed. 
