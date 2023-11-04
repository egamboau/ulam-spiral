# Prime Number definitions.
Mathematically speaking, a prime number (or a prime) is a natural number greater than 1 that is not a product of two smaller natural numbers (From wikipedia: https://en.wikipedia.org/wiki/Prime_number). In more general words, a prime number can be divided only by 1 and itself. 

The process of checking if a number is prime or not is called "primality tests". Even when factorization for a number is quite heavy for computers, this tests usually run at polynomial in the size of the input.

## Checking if a number is prime

### Basic concepts

There are multiple tests to check if a number is prime or not. 

One basic test, which was used first for the Python script, is that, for any given input $n \ge 2$, $n$ is checked wether it is divisible by any natural number in the range $[2,\sqrt{n}]$ If the process found a division with no reminder, the number is not prime. Otherwise, the number is considered prime. 

If you consider this approach, however, it may be analyzing some factors twice. As a basic example, if we need to check if 20 is prime, the algoritm go over the numbers 1, 2, 3, and 4. However, if the number 4 can divide evenly the number 20 (which it can, $4 \times 5 = 20$), then 2 can ($2 \times 10 = 20$)

One very good optimization for this algorithm, found in the Wikipedia link, uses the definition of primorial, and the fact that prime integers can be represented by them. 

For any number $n$, its primorial, $n\\#$, is defined as the product of all the primes not grater than $n$. In other words:

$$
n\\# = \begin{cases}
        1 & \text{if } n = 0,1 \\
        (n-1)\\# \times n & \text{if n is prime} \\
        (n-1)\\# \times & \text{if n is not prime} \\
    \end{cases}
$$

Also, all primes greater than $n\\#$ can be represented in the form $n\\# \times k + i$, where $i$ represents all the coprimes for $n$, and $i \lt n\\#$. Note that two numbers are coprimes if their greatest common divisor is 1. 

By using $3\\# = 6$, we can represent the numbers with $6 \times k + i$, for $i$ in $i = 0,1,2,3,4,5$. However, $2$ divides $6 \times k + 0$, $6 \times k + 2$ and $6 \times k + 4$, Furhtermore, $3$ divides $6 \times k + 3$. This leave us only with the formula  $6 \times k + 1$, which will represent all of the primes greather than 6. Note that this also generates composite numbers, so the test needs to be performed. However, it will automatically ignore already tested factors. 

Another way to make the comparison shorter, instead of checking $1 \le i \le n\\#$, the test can be performed on $1 \le i \le \frac{n\\#}{2}$, and change the formula to $6 \times k \pm 1$. 

### The algorithm. 

The process is very simple: 
1. If the number is either 2 or 3, is prime
2. If the number is not divisible by 2 or 3, check all the numbers in the form $6k\pm1\sqrt{n}$. If a divisor is found, the number is not prime
3. When all of the numbers are tested, and no divisor is found, the number is prime
