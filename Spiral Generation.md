# Calculating the position of the number
The idea for this part, is to get a position, determine by X and Y coordinates, for a given number, and simulate "walking" over the spiral until the position is reached. However, for efficiency, is better to use math and calculate, rather than for loops for the actual traversal.

Note that, as this is use for computation, positions and coordinates are zero-based, and the origin $(0,0)$ is located on the top-left corner ofthe matrix

Also, the $x$ component counts columns from left to right, as the $y$ component counts columns from top to bottom. Note that, when used on code, the components needs to be reversed, as the array access selects the pointer for the column first, and then the row.

The following sections show the process for the calculations, in parts

## Spiral origin

First, Let's start with the most basic matrix, a $1x1$: 

$$
\begin{pmatrix}
1 \\
\end{pmatrix}
$$

This is the trivial case, as it is just 1 nunmber, and 1 position. However, in case of a $2x2$ matrix, things change a bit:
$$
\begin{pmatrix}
4 & 3 \\
1 & 2
\end{pmatrix}
$$

One key fact that can be determined here, is that the origin from the spiral shifted a bit, is not on the real center of the matrix. However, checking a $3x3$ matrix:
$$
\begin{pmatrix}
5 & 4 & 3 \\
6 & 1 & 2  \\
7 & 8 & 9  \\
\end{pmatrix}
$$

The origin is now the center of the matrix. Following this pattern, to get the coordinates for the origin of the spiral $(x_o,y_o)$ of a given matrix size, the formula is as follows:
$$
\begin{align}
x_o = \begin{cases}
        \lfloor \frac {n}{2} \rfloor & \text{if n is odd} \\
        \frac {n}{2} -1 & \text{otherwise} \\
    \end{cases}
&&
y_o = \begin{cases}
        \lfloor \frac {n}{2} \rfloor & \text{if n is odd} \\
        \frac {n}{2} & \text{otherwise} \\
    \end{cases}
\end{align}
$$

Using the formulas to get the center on our samples:
$$
\begin{align*}
\text{For }n = 1 : &&
&x = \lfloor\frac{1}{2}\rfloor = 0 ;&&
&y = \lfloor\frac{1}{2}\rfloor = 0 
\\
\text{For }n = 2: &&
&x = \frac{2}{2} -1 = 0 &&
&y = \frac{2}{2} = 1
\\
\text{For }n = 3 : &&
&x = \lfloor\frac{3}{2}\rfloor = 1 ;&&
&y = \lfloor\frac{3}{2}\rfloor = 1 
\\
\end{align*}
$$

Which matches the samples shown. 

## Get the destination

To get where a given number $z$ will be stored when the path is traversed, two components are needed:
1. How many perfect squares we have on $z$. We will call this number $s$, and it must satisfy that $s^2$ is the nearest to $z$, and also, $s^2 < z$
2. The difference $d = z - s^2 $ so we can calculate the final steps. 

For this section, take into account a $6x6$ matrix M:
$$
M = \begin{pmatrix}
36 & 35 & 34 & 33 & 32 & 31 \\
17 & 16 & 15 & 14 & 13 & 30 \\
18 & 5  & 4  & 3  & 12 & 29 \\
19 & 6  & 1  & 2  & 11 & 28 \\
20 & 7  & 8  & 9  & 10 & 27 \\
21 & 22 & 23 & 24 & 25 & 26 \\
\end{pmatrix}
$$

Again, some of the values follow patterns that can be used to make some calculation easier. The process is as follows:

### Calculate s:

For this:  $s = \lfloor\sqrt{z}\rfloor$

### Calculate the x component for the position of s in the spiral

For a given number $s$, and the spiral origin $(x_o,y_o)$, the following values $(x_{s},y_s)$ can be calculated
$$
\begin{align*}
\text{for } s &= 1, x_s = x_o \\
\text{for } s &= 2, x_s = x_o \\
\text{for } s &= 3, x_s = x_o + 1 \\
\text{for } s &= 4, x_s = x_o + 1 - 2 \\
\text{for } s &= 5, x_s = x_o + 1 - 2 + 3 \\
\text{for } s &= 6, x_s = x_o + 1 - 2 + 3 - 4 \\
\end{align*}
$$

Based on the pattern, we can derive the following formula:
$$
\begin{align}
x_s = \begin{cases}
        x_o & \text{if } s = 1 \text{ or } s=2 \\
        x_o + \sum_{i = 1}^{s - 2} -1^{i+1}i & \text{if } s \gt 2\\
    \end{cases}
\end{align}
$$

Which means that the component $x_s$ can be calculated using an alternating series. Fortunately, the result is known:

$$
\sum_{i = 1}^{n} -1^{i+1}i = \begin{cases}
                             \frac{-n}{2} & \text{if n is even} \\
                             \frac{n+1}{2} & \text{if n is odd}\\
                             \end{cases}
$$

### Calculate the y component for the position of s in the spiral

Likewise, for a given number $s$, the component $y_s$ can be generated as follows:
$$
\begin{align*}
\text{for } s &= 1, y_s = x_o \\
\text{for } s &= 2, y_s = x_o - 1 \\
\text{for } s &= 3, y_s = x_o - 1 + 2 \\
\text{for } s &= 4, y_s = x_o - 1 + 2 -3\\
\text{for } s &= 5, y_s = x_o - 1 + 2 - 3 \\
\text{for } s &= 6, y_s = x_o - 1 + 2 - 3 + 4 - 5 \\
\end{align*}
$$

Which, again following the pattern, can be rewritten to:
$$
\begin{align}
y_s = \begin{cases}
        y_o & \text{if } s = 1 \\
        y_o - \sum_{i = 1}^{s - 1} -1^{i+1}i & \text{if } s \gt 1\\
    \end{cases}

\end{align}
$$

### Getting the final position. 

With $s$ calculated, and the position $(x_s, y_s)$ retrieved, the next step is to get the offset for the numbers, this time, from $(x_s and y_s)$:

As stated above, first we got the distance between the number $z$ and the square number $s^2$: 
$$
\begin {align}
d = z - s^2
\end{align}
$$

In the case that $d=0$ the number is in position, so it can be just put in $(x_s,y_s)$, as it means that is a perfect square number. 

If, in the other hand, the number is not zero, more calculations are needed. For this, let's refer to $M$ again:

$$
M = \begin{pmatrix}
36 & 35 & 34 & 33 & 32 & 31 \\
17 & 16 & 15 & 14 & 13 & 30 \\
18 & 5  & 4  & 3  & 12 & 29 \\
19 & 6  & 1  & 2  & 11 & 28 \\
20 & 7  & 8  & 9  & 10 & 27 \\
21 & 22 & 23 & 24 & 25 & 26 \\
\end{pmatrix}
$$

Also, let's assume that $s = 2$, and try to position the numbers 5 through 8 on the matrix:

* number $5$ is located on the left of number $4$.
* number $6$ is located in the same column as $5$, however, is on the row below
* number $7$ is, again, on the same column as $5$. However, the row is decreased again.
* number $8$ is on the same row as number $7$, but the column changed again. This is because the spiral turns to the right

Things that can be denoted from this analysis are:
1. The next number will be on the left of the position of the perfect square
2. Subsequent numbers will increase the row, until the spiral takes $s$ steps
3. After this, the rows are incremented again, until another $s - 1$ steps are taken.

We can verify again if we got $s = 4$:
1. $17$ is on the left of $16$
2. The rows go down until $21$ which is $(17 + 4)$
3. The colums go left until $24$ which is $(21 + 4 - 1)$

Using this, we can work out the final positions $(x_d, y_d)$ for any $s$, given that $s$ is even, as follows:
$$
x_d = \begin{cases}
        x_s - 1 & \text{if } d \leq s - 1  \\
        x_s - 1 + d - s  - 1 & \text{otherwise.} 
    \end{cases}
\\
y_d = \begin{cases}
        y_s + d  - 1 & \text{if } d \leq s^2 - 1  \\
        y_s + s & \text{otherwise.} 
    \end{cases}
$$

Now, let's assume a value $s = 3$. Placing the numbers from 10 15 shows us that:
* Number 10 is on the right column from number 9
* Number 11 is on the same column as 10, however, the row is increased by 1
* Number 12 is on the same column as 11, however, the row is increased by 1
* Number 13 is on the same column as 12, however, the row is increased by 1
* Number 14 is on the same row as 13, this time, the column is decreased by 1
* Number 14 is on the same row as 14, this time, the column is decreased by 1 again

We can see a similar behavior as with the even values of $s$:
1. The next number will be on the right of the position of the perfect square
2. Subsequent numbers will decrease the row, until the spiral takes $s$ steps
3. After this, the rows are decremented again, until another $s - 1$ steps are taken.

For the next odd value of $s = 5$, the pattern holds:
* 26 is on the right column from $25$
* The rows increase until $31$ which is $26 + 5$
* The colum decreases again until $35$ which is $31 + 5 - 1$

With this, the following formulas can be derived:
$$
x_d = \begin{cases}
        x_s + 1 & \text{if } d \leq s - 1  \\
        x_s + 1 - d - s  - 1 & \text{otherwise.} 
    \end{cases}
\\
y_d = \begin{cases}
        y_s - d  + 1 & \text{if } d \leq s - 1  \\
        y_s - s & \text{otherwise.} 
    \end{cases}
$$

The two expresions can be fused, as the only changes from those formulas are signs, which can be easily flipped over with exponents:
$$
x_d = \begin{cases}
        x_s + (1)(-1)^{s + 1} & \text{if } d \leq s - 1  \\
        x_s + 1(-1)^{s + 1} + (d - s  - 1)(-1)^{s} & \text{otherwise.} \\
    \end{cases}
\\
y_d = \begin{cases}
        y_s + (d + 1)(-1)^{s + 1} & \text{if } d \leq s - 1  \\
        y_s - (s)(-1)^{s + 1} & \text{otherwise.} 
    \end{cases}
$$

With this, we got the coordinates $(x_s,y_s)$ needed for the placement of the number. Repeating this for all the numbers on the possible values, the spiral will be formed. 

# Process Examples

Let's calculate some values from a matrix, to check that the correct positions are beign generated correcly. As we havve been using on this document, let's calculate different numbers for a $6x6$ matrix. 

## Spiral Origin
$$
\begin{align*}
x_o &= \begin{cases}
        \lfloor \frac {n}{2} \rfloor & \text{if n is odd} \\
        \frac {n}{2} -1 & \text{otherwise} \\
    \end{cases} \\

    &= \frac {6}{2} -1 \\
    &= 3 - 1 \\
    &= 2

\end{align*}
$$
$$
\begin{align*}
y_o &= \begin{cases}
        \lfloor \frac {n}{2} \rfloor & \text{if n is odd} \\
        \frac {n}{2} & \text{otherwise} \\
    \end{cases} \\
    &= \frac {6}{2} \\
    &= 3
\end{align*}
$$

So, $(x_o,y_o) = (2,3)$

## Position for number 1:
As a first example, let's put number 1 to our matrix, which should go on the origin. So: 

$$
\begin{align*}
s &= \lfloor\sqrt{1}\rfloor \\
  &= 1
\end{align*}
$$
For $(x_s,y_s)$:
$$
\begin{align*}
x_s &= \begin{cases}
        x_o & \text{if } s = 1 \text{ or } s=2 \\
        x_o + \sum_{i = 1}^{s - 2} -1^{i+1}i & \text{if } s \gt 2\\
    \end{cases} \\
    &=x_o \\
    &= 2
\\
y_s &= \begin{cases}
        y_o & \text{if } s = 1 \\
        x_o + \sum_{i = 1}^{s - 1} -1^{i+1}i & \text{if } s \gt 1\\
    \end{cases} \\
    &= y_o \\
    &= 3

\end{align*}
$$
So, $(x_s,y_s) = (2,3)$

Finally: 
$$
\begin{align*}
d &= z - s^2 \\
  &= 1 - 1^2 \\
  &= 0
\end{align*}
$$
As $d = 0$, the number the final position for the number is $(x_s,y_s) = (2,3)$, matching the origin of the spiral, as expected.

## Position for number 33
Now, let's calculate something not as trivial, the number 33 on the matrix:
$$
\begin{align*}
s &= \lfloor\sqrt{33}\rfloor \\
  &= 5
\end{align*}
$$
For $(x_s,y_s)$:
$$
\begin{align*}
x_s &= \begin{cases}
        x_o & \text{if } s = 1 \text{ or } s=2 \\
        x_o + \sum_{i = 1}^{s - 2} -1^{i+1}i & \text{if } s \gt 2\\
    \end{cases} \\
    &= 2 + \sum_{i = 1}^{5 - 2} -1^{i+1}i\\
    &= 2 + \sum_{i = 1}^{3} -1^{i+1}i\\
    &= 2 + \begin{cases}
                             \frac{-n}{2} & \text{if n is even} \\
                             \frac{n+1}{2} & \text{if n is odd}\\
                             \end{cases}\\
    &= 2 + \frac{3+1}{2} \\
    &= 2 + \frac{4}{2} \\
    &= 2 + 2 \\
    &= 4 \\
\\
y_s &= \begin{cases}
        y_o & \text{if } s = 1 \\
        y_o - \sum_{i = 1}^{s - 1} -1^{i+1}i & \text{if } s \gt 1\\
    \end{cases} \\
    &= 3 - \sum_{i = 1}^{5 - 1} -1^{i+1}i \\
    &= 3 - \sum_{i = 1}^{4} -1^{i+1}i \\
    &= 3 - \begin{cases}
                             \frac{-n}{2} & \text{if n is even} \\
                             \frac{n+1}{2} & \text{if n is odd}\\
                             \end{cases}\\
    &= 3 - \frac{-4}{2}\\
    &= 3 - -2\\
    &= 5\\

\end{align*}
$$
So, $(x_s,y_s) = (4,5)$

Finally: 
$$
\begin{align*}
d &= z - s^2 \\
  &= 33 - 5^2 \\
  &= 33 - 25 \\
  &= 8
\end{align*}
$$

As $d \ne 0$, we need some extra calculations:
$$
\begin{align*}

x_d &= \begin{cases}
        x_s + (1)(-1)^{s + 1} & \text{if } 8 \leq 5 - 1  \\
        x_s + 1(-1)^{s + 1} + (d - s  - 1)(-1)^{s} & \text{otherwise.}
    \end{cases} \\
    &= 4 + 1(-1)^{5 + 1} + (8 - 5  - 1)(-1)^{5} \\
    &= 4 + 1(-1)^{6} + (8 - 5  - 1)(-1)^{5} \\
    &= 4 + 1(1) + (8 - 5  - 1)(-1) \\
    &= 4 + 1 + (2)(-1) \\
    &= 4 + 1 - 2 \\
    &= 3 \\
\\

y_d &= \begin{cases}
        y_s + (d + 1)(-1)^{s + 1} & \text{if } 8 \leq 5 - 1  \\
        y_s - (s)(-1)^{s + 1} & \text{otherwise.} 
    \end{cases} \\
    &= 5 - (5)(-1)^{5 + 1} \\
    &= 5 - (5)(-1)^{6} \\
    &= 5 - (5)(1) \\
    &= 5 - 5 \\
    &= 0 \\
\end{align*}
$$

So, the position $(x,y)$ for the number 33 is $(3,0)$, matching the position based on the $M$ matrix.

## Position for number 24

As a last sample, let's calcualte the position for number 24:
$$
\begin{align*}
s &= \lfloor\sqrt{24}\rfloor \\
  &= 4
\end{align*}
$$
For $(x_s,y_s)$:
$$
\begin{align*}
x_s &= \begin{cases}
        x_o & \text{if } s = 1 \text{ or } s=2 \\
        x_o + \sum_{i = 1}^{s - 2} -1^{i+1}i & \text{if } s \gt 2\\
    \end{cases} \\
    &= 2 + \sum_{i = 1}^{4 - 2} -1^{i+1}i\\
    &= 2 + \sum_{i = 1}^{2} -1^{i+1}i\\
    &= 2 + \begin{cases}
                             \frac{-n}{2} & \text{if n is even} \\
                             \frac{n+1}{2} & \text{if n is odd}\\
                             \end{cases}\\
    &= 2 + \frac{-2}{2} \\
    &= 2 + -1 \\
    &= 1 \\
\\
y_s &= \begin{cases}
        y_o & \text{if } s = 1 \\
        y_o - \sum_{i = 1}^{s - 1} -1^{i+1}i & \text{if } s \gt 1\\
    \end{cases} \\
    &= 3 - \sum_{i = 1}^{4 - 1} -1^{i+1}i \\
    &= 3 - \sum_{i = 1}^{3} -1^{i+1}i \\
    &= 3 - \begin{cases}
                             \frac{-n}{2} & \text{if n is even} \\
                             \frac{n+1}{2} & \text{if n is odd}\\
                             \end{cases}\\
    &= 3 - \frac{3+1}{2}\\
    &= 3 - \frac{4}{2}\\
    &= 3 - 2\\
    &= 1\\

\end{align*}
$$
So, $(x_s,y_s) = (1,1)$

Finally: 
$$
\begin{align*}
d &= z - s^2 \\
  &= 24 - 4^2 \\
  &= 24 - 16 \\
  &= 8
\end{align*}
$$

As $d \ne 0$, we need some extra calculations:
$$
\begin{align*}

x_d &= \begin{cases}
        x_s + (1)(-1)^{s + 1} & \text{if } 8 \leq 4 - 1  \\
        x_s + 1(-1)^{s + 1} + (d - s  - 1)(-1)^{s} & \text{otherwise.}
    \end{cases} \\
    &= 1 + 1(-1)^{4 + 1} + (8 - 4  - 1)(-1)^{4} \\
    &= 1 + 1(-1)^{5} + (8 - 4  - 1)(-1)^{4} \\
    &= 1 + 1(-1) + (8 - 4  - 1)(1) \\
    &= 1 + -1 + (8 - 4  - 1) \\
    &= 1 - 1 + 3 \\
    &= 3 \\
    
\\

y_d &= \begin{cases}
        y_s + (d + 1)(-1)^{s + 1} & \text{if } 8 \leq 4 - 1  \\
        y_s - (s)(-1)^{s + 1} & \text{otherwise.} 
    \end{cases} \\
    &= 1 - (4)(-1)^{4 + 1}\\
    &= 1 - (4)(-1)^{5}\\
    &= 1 - (4)(-1)\\
    &= 1 - -4\\
    &= 1 + 4\\
    &= 5\\

\end{align*}
$$

So, the position $(x,y)$ for the number 24 is $(3,5)$, matching the position based on the $M$ matrix.