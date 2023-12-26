module.exports = ({ number, x_center, y_center }) => {
    if (number === 1){
        return {number: number, x: x_center, y: y_center, n_is_prime: false}
    } else {
        //Check how many squares we had completed. ith this, we can check where the number should be
        //by "walking" on the spiral
        const complete_squares = Math.trunc(Math.sqrt(number))
        // get the position for the last completed square.
        // x is the same if there are 1 or 2 complete squares
        // however, y is maintained only if the complete square is 1
        // so, we set y on a ternary op on that specific case. Oterwise, just use the calculation as is needed
        let x = x_center
        let y = complete_squares === 2? y_center -1 : y_center

        const distance_from_last_square = number - Math.pow(complete_squares, 2)
        if(complete_squares > 2){
            x = x + calculate_first_natural_numbers_alternate_series(complete_squares - 2)
            y = y - calculate_first_natural_numbers_alternate_series(complete_squares - 1)
        }

        // if the distance is 0, we already have x and y to return, using the previous calculations.
        // however, we need to "walk" over the spiral to cover any other gap
        if (distance_from_last_square != 0) {
            // for odd numbers, the sign of the offsets need to be negative
            let y_offset = -1
            let x_offset = -1
            // the walk direction changes if the complete squares is even, or odd.
            if (distance_from_last_square <= complete_squares ){
                x_offset = Math.pow(-1, complete_squares + 1)
                y_offset = ((distance_from_last_square* ((-1)**(complete_squares))) +
                ((-1)**(complete_squares + 1)))
            } else {
                x_offset = ((-1)**(complete_squares + 1)) + ((distance_from_last_square- complete_squares - 1) * ((-1)**complete_squares));
                y_offset = -(complete_squares * ((-1)**(complete_squares + 1)));
            }
            y += y_offset
            x += x_offset
        }

        // if the number is 2 or 3, the number is prime. 
        // Also, if the number not divisible by 2 or 3, the number is prime 
        let is_prime = (number <= 3) || (number % 2 && number %3)

        if (is_prime) {
            //check with the formula 6k+-1.
            //the first value is (6*1) - 1, which is 5
            //also, (6*1) + 1, which is 7, or 5 + 2

            let current_value = 5
            while (current_value <= complete_squares) {
                if (!(number % current_value) || !(number % (current_value + 2))) {
                    // found a factor, the number is not prime
                    is_prime = false
                    break
                }
                // Adding 1 to k is the same as adding 6 to the current value.
                current_value += 6
            }
        }
        return {number: number, x: x, y: y, n_is_prime: !!is_prime}
    }
  };


isOdd = (number) => {
    return (number & 1) === 1
}

calculate_first_natural_numbers_alternate_series = (n) => {
    if(isOdd(n)) {
        return (n + 1) / 2
    } else {
        return (-n) / 2
    }
}
    
    