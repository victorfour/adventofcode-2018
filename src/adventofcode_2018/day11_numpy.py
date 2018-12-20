import numpy as np
import time


def np_hundreds(number):
    return np.floor_divide(np.mod(number, 1000), 100)

def compute_grid(serial_number, width = 300, height = 300):
    xs = np.arange(1, 1 + width).reshape((1, -1))
    ys = np.arange(1, 1 + height).reshape((-1, 1))
    rack_ids = xs + 10
    grid = np_hundreds(((rack_ids * ys) + serial_number) * rack_ids) - 5
    return grid


def hundreds(number):
    return (number % 1000) // 100

def power_level(serial_number, x, y):
    rack_id = x + 10
    return  hundreds((rack_id * y + serial_number) * rack_id) - 5

#print(compute_grid(8)[2,4])
#print(compute_grid(57)[121,78])
#print(compute_grid(39)[216,195])
#print(compute_grid(71)[100,152])

def blur(grid, filter_size = 3):
    dim, _ = grid.shape
    blurred_dim = dim - filter_size + 1
    blurred = np.zeros((blurred_dim, blurred_dim))
    for x in range(blurred_dim):
        for y in range(blurred_dim):
            blurred[x, y] = np.sum(grid[x:x+filter_size, y:y+filter_size])
    return blurred


def argmax_grid(grid):
    y,x = tuple(map(lambda x: x+1, np.unravel_index(np.argmax(grid), grid.shape)))
    return (x,y)


def part1(puzzle_input):
    return argmax_grid(blur(compute_grid(puzzle_input)))

#print(part1(18))
#print(part1(42))
    

def argmax_grids(grids, filter_min):
    max_value_grid = np.argmax(np.array([np.amax(grid) for grid in grids]))
    x,y = argmax_grid(grids[max_value_grid])
    return (x,y,filter_min + max_value_grid)



def part2(puzzle_input, filter_min=1, filter_max=300):
    power_levels = compute_grid(8868)
    blurred = [blur(power_levels, filter_size) for filter_size in range(filter_min, filter_max + 1)]
    return argmax_grids(blurred, filter_min)


if "__main__" == __name__:
    start = time.time()
    part1_res = part1(8868)
    end = time.time()
    print("part1:", part1_res, " (elapsed time: ", end - start, "secs)")

    start = time.time()
    part2_res = part2(8868, 1, 300)
    end = time.time()
    print("part2:", part2_res, " (elapsed time: ", end - start, "secs)")
    
