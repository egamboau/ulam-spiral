import numpy as np

import concurrent.futures
import multiprocessing
import logging
import argparse

##logger config
log = logging.getLogger()
log.setLevel(logging.getLevelName('INFO'))
log_formatter = logging.Formatter("%(asctime)s [%(processName)s]-[%(levelname)s] [%(threadName)s]: %(message)s") # I am log.infoing thread id here

console_handler = logging.StreamHandler()
console_handler.setFormatter(log_formatter)
log.addHandler(console_handler)




def isOdd(number)->bool:
    """
    Method that returns if a number is even or odd. Checking this by using a bitwise and, 
    if the less-significan bit is 1, then the number is odd. Otherwise, the number is even

    As is implemented here, the operation returns actually true (i.e a non-zero value) if the number is odd.
    """
    return np.bitwise_and(number, 1)

def calculate_first_natural_numbers_alternate_series(n:int) -> int:
    """
    Function that calculates the first N natural numbers, alternating.
    """
    if(isOdd(n)):
        return (n + 1) // 2
    else:
        return (-n) // 2

def get_position_on_spiral_for_number(n:int, x_center:int, y_center:int):
    
    if n == 1:
        return (y_center,x_center, False)
    else:
        #Check how many squares we had completed. ith this, we can check where the number should be
        #by "walking" on the spiral
        complete_squares = np.sqrt(n).astype(int)
        #get the position for the last completed square.
        #x is the same if there are 1 or 2 complete squares
        #however, y is maintained only if the complete square is 1
        #so, we set y on a ternary op on that specific case. Oterwise, just use the calculation as is needed
        x = x_center
        y = y_center -1 if complete_squares == 2 else y_center

        distance_from_last_square = n - np.power(complete_squares, 2)

        if complete_squares > 2:
            x = x + calculate_first_natural_numbers_alternate_series(complete_squares - 2)
            y = y - calculate_first_natural_numbers_alternate_series(complete_squares - 1)
        
        ##if the distance is 0, we already have x and y to return, using the previous calculations.
        ##however, we need to "walk" over the spiral to cover any other gap
        if distance_from_last_square != 0:
            #for odd numbers, the sign of the offsets need to be negative. as True is considered 1, raising 
            #-1 to the value of the isOdd function, we get a positive/negative flip
            is_odd_number = isOdd(complete_squares)
            sign_constant = np.power(-1, is_odd_number)
            ##the walk direction changes if the complete squares is even, or odd.
            if distance_from_last_square <= complete_squares +1:
                y_offset = np.multiply((distance_from_last_square -1), sign_constant)
                x_offset = -sign_constant
            else:
                x_offset = np.multiply(np.subtract(distance_from_last_square, np.sum((complete_squares,1))), sign_constant) - sign_constant
                y_offset = np.multiply(complete_squares, sign_constant)
            
            y += y_offset
            x += x_offset

        #check if this number is prime, we can reuse some of the math operations used for this calculation
        is_prime = True
        for current_number in range(2, complete_squares + 1):
            if n % current_number == 0:
                #found a factor, the number is not prime
                is_prime = False
                break
        return (y,x,is_prime)

def main(array_size):
    #to hold our data. As this exercise is just for numbers greater than 1, fill with 0 will give us a good way
    # to know when a cell is emptu

    array = np.zeros(shape=(array_size, array_size), dtype="U1")

    ##the center of the spiral changes, if the number is even or odd. Odd numbers need to be adjusted a bit, so the center is still ok
    quotient, reminder = np.divmod(array_size, 2)
    x_center = -1
    y_center = -1
    y_center = quotient
    if reminder == 0:
        #adjust the center, by going one to the left for the x axis, so we can go 
        x_center = quotient - 1
    else:
        x_center = quotient       

    futures = {}

    #create this inner function as a listener for the futures, so it will populate the array
    #as it goes. 
    def populate_array_based_on_result(task_result:concurrent.futures.Future):
        task_info = futures[task_result]
        log.info(f"Completed task for number {task_info}")
        x,y,n_is_prime =  task_result.result()
        array[x][y] = "*" if n_is_prime else "-"


    with concurrent.futures.ProcessPoolExecutor(multiprocessing.cpu_count()) as executor:
        for number in range(1,(array_size ** 2) +1):
            log.info(f"submitting {number}")
            future_task = executor.submit(get_position_on_spiral_for_number, number, x_center, y_center)
            future_task.add_done_callback(populate_array_based_on_result)
            futures[future_task] = number
        executor.shutdown(wait=True, cancel_futures=False)
            

    rows_joined = ["".join(row) for row in array]
    rows_joined = '\n'.join(rows_joined) 
    print(rows_joined)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Prints Ulam spiral on screen. Logs to stderr so output can be redirected')
    parser.add_argument('size', metavar='N', type=int, help="The size of the side of the square. The spiral will be of size N squared")
    args = parser.parse_args()
    main(args.size)