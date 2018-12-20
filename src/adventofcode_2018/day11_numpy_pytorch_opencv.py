import cv2
import numpy as np
import time
import torch
import torch.nn.functional as F


#part1
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


def np_blur(grid, filter_size = 3):
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


def np_part1(puzzle_input):
    return argmax_grid(np_blur(compute_grid(puzzle_input)))


def torch_blur(grid, filter_size = 3):
    dim = grid.size()[-1]
    out_dim = dim - filter_size + 1
    filters = torch.ones(1,1,filter_size,filter_size)
    blurred = F.conv2d(grid, filters, padding = 0).view(out_dim, out_dim).long().numpy()
    return blurred


def torch_part1(puzzle_input):
    grid = compute_grid(puzzle_input)
    dim, _ = grid.shape
    torch_grid = torch.from_numpy(grid).view(1, 1, dim, dim).float()
    return argmax_grid(torch_blur(torch_grid))

def cv_blur(grid, filter_size = 3):
    return cv2.boxFilter(grid, ddepth=-1, ksize=(filter_size, filter_size), anchor=(0,0), normalize=False, borderType = cv2.BORDER_ISOLATED)
    return blurred

def cv_part1(puzzle_input):
    return argmax_grid(cv_blur(compute_grid(puzzle_input)))

#part2

def argmax_grids(grids, filter_min):
    max_value_grid = np.argmax(np.array([np.amax(grid) for grid in grids]))
    x,y = argmax_grid(grids[max_value_grid])
    return (x,y,filter_min + max_value_grid)

def np_part2(puzzle_input, filter_min=1, filter_max=300):
    power_levels = compute_grid(puzzle_input)
    blurred = [np_blur(power_levels, filter_size) for filter_size in range(filter_min, filter_max + 1)]
    return argmax_grids(blurred, filter_min)


def torch_part2(puzzle_input, filter_min=1, filter_max=300):
    grid = compute_grid(puzzle_input)
    dim, _ = grid.shape
    torch_grid = torch.from_numpy(grid).view(1, 1, dim, dim).float()
    blurred = [torch_blur(torch_grid, filter_size) for filter_size in range(filter_min, filter_max + 1)]
    return argmax_grids(blurred, filter_min)

def cv_part2(puzzle_input, filter_min=1, filter_max=300):
    power_levels = compute_grid(puzzle_input)
    blurred = [cv_blur(power_levels, filter_size) for filter_size in range(filter_min, filter_max + 1)]
    return argmax_grids(blurred, filter_min)


if "__main__" == __name__:
    print("\n")
    start = time.time()
    part1_res = np_part1(8868)
    end = time.time()
    print("part1:", part1_res, " ([numpy] elapsed time: ", end - start, "secs)")
    #part1: (241, 40)  ([numpy] elapsed time:  0.6327872276306152 secs)

    start = time.time()
    part1_res = torch_part1(8868)
    end = time.time()
    print("part1:", part1_res, " ([torch] elapsed time: ", end - start, "secs)")
    #part1: (241, 40)  ([torch] elapsed time:  0.006007194519042969 secs)

    start = time.time()
    part1_res = cv_part1(8868)
    end = time.time()
    print("part1:", part1_res, " ([opencv] elapsed time: ", end - start, "secs)")
    #part1: (241, 40)  ([opencv] elapsed time:  0.017156124114990234 secs)


    max_filter_size = 300

    start = time.time()
    part2_res = np_part2(8868, 1, max_filter_size)
    end = time.time()
    print("part2:", part2_res, " ([numpy] elapsed time: ", end - start, "secs)")
    #part2: (166, 75, 12)  ([numpy] elapsed time:  145.2938311100006 secs)

    start = time.time()
    part2_res = torch_part2(8868, 1, max_filter_size)
    end = time.time()
    print("part2:", part2_res, " ([torch] elapsed time: ", end - start, "secs)")
    #part2: (166, 75, 12)  ([torch] elapsed time:  258.4134259223938 secs)
    
    start = time.time()
    part2_res = cv_part2(8868, 1, max_filter_size)
    end = time.time()
    print("part2:", part2_res, " ([opencv] elapsed time: ", end - start, "secs)")
    #part2: (166, 75, 12)  ([opencv] elapsed time:  0.17168807983398438 secs)

