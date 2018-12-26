import cv2
import numpy as np
import time
import torch
import torch.nn.functional as F
from numba import jit


#part1
@jit
def np_hundreds(number):
    return np.floor_divide(np.mod(number, 1000), 100)

@jit
def compute_grid(serial_number, width = 300, height = 300):
    xs = np.arange(1, 1 + width).reshape((1, -1))
    ys = np.arange(1, 1 + height).reshape((-1, 1))
    rack_ids = xs + 10
    grid = np_hundreds(((rack_ids * ys) + serial_number) * rack_ids) - 5
    return grid


@jit
def hundreds(number):
    return (number % 1000) // 100

@jit
def power_level(serial_number, x, y):
    rack_id = x + 10
    return  hundreds((rack_id * y + serial_number) * rack_id) - 5

def argmax_grid(grid):
    y,x = tuple(map(lambda x: x+1, np.unravel_index(np.argmax(grid), grid.shape)))
    return (x,y)

@jit
def np_sep_blur(grid, filter_size = 3):
    dim, _ = grid.shape
    blurred_dim = dim - filter_size + 1
    blurx = np.zeros((dim, blurred_dim))
    blurred = np.zeros((blurred_dim, blurred_dim))
    for x in range(dim):
        for y in range(blurred_dim):
            blurx[x, y] = np.sum(grid[x,y:y+filter_size])
    for x in range(blurred_dim):
        for y in range(blurred_dim):
            blurred[x, y] = np.sum(blurx[x:x + filter_size,y])
    return blurred

@jit
def np_blur(grid, filter_size = 3):
    dim, _ = grid.shape
    blurred_dim = dim - filter_size + 1
    blurred = np.zeros((blurred_dim, blurred_dim))
    for x in range(blurred_dim):
        for y in range(blurred_dim):
            blurred[x, y] = np.sum(grid[x:x+filter_size,y:y+filter_size])
    return blurred


@jit
def torch_sep_blur(grid, filter_size = 3):
    dim = grid.shape[1]
    out_dim = dim - filter_size + 1
    filters = torch.ones(1,1,1,filter_size)
    blurx = F.conv2d(grid, filters, padding = 0, stride=(1,1)).transpose(2,3)
    blurred = F.conv2d(blurx, filters, padding = 0, stride=(1,1)).transpose(2,3)
    return blurred.squeeze().long().numpy()


@jit
def torch_blur(grid, filter_size = 3):
    dim = grid.shape[1]
    out_dim = dim - filter_size + 1
    filters = torch.ones(1,1,filter_size,filter_size)
    blurred = F.conv2d(grid, filters, padding = 0, stride=(1,1))
    return blurred.squeeze().long().numpy()

@jit
def cv_blur(grid, filter_size = 3):
    return cv2.boxFilter(grid, ddepth=-1, ksize=(filter_size, filter_size), anchor=(0,0), normalize=False, borderType = cv2.BORDER_ISOLATED)



@jit
def np_to_torch(grid, dim=300):
    return torch.from_numpy(grid).view(1, 1, dim, dim).float()

@jit
def part1(puzzle_input, blur_fun=np_sep_blur, transform = lambda x: x):
    grid = compute_grid(puzzle_input)
    return argmax_grid(blur_fun(transform(grid)))


#part2

def argmax_grids(grids, filter_min):
    max_value_grid = np.argmax(np.array([np.amax(grid) for grid in grids]))
    x,y = argmax_grid(grids[max_value_grid])
    return (x,y,filter_min + max_value_grid)

def part2(puzzle_input, filter_min=1, filter_max=300, blur_fun=np_sep_blur, transform=lambda x:x):
    power_levels = transform(compute_grid(puzzle_input))
    blurred = [blur_fun(power_levels, filter_size) for filter_size in range(filter_min, filter_max + 1)]
    return argmax_grids(blurred, filter_min)


if "__main__" == __name__:
    print("\n")

    puzzle_input = 8868
    max_filter_size = 300

    algs = {"numpy ": {"fun" : np_blur,
                      "transform": lambda x:x},
            "np_sep": {"fun" : np_sep_blur,
                       "transform": lambda x:x},
            "torch ": {"fun" : torch_blur,
                       "transform": np_to_torch},
            "torch_sep": {"fun" : torch_sep_blur,
                          "transform": np_to_torch},
            "opencv": {"fun" : cv_blur,
                       "transform": lambda x:x}}

    for alg, params in algs.items():
        start = time.time()
        res = part1(puzzle_input, blur_fun=params['fun'], transform=params['transform'])
        end = time.time()
        print("part1:{} [{}]\telapsed time: {}secs)".format(res, alg, end-start))

    algs = {"np_sep": {"fun" : np_sep_blur,
                       "transform": lambda x:x},
            "torch_sep": {"fun" : torch_sep_blur,
                          "transform": np_to_torch},
            "opencv": {"fun" : cv_blur,
                       "transform": lambda x:x}}

    for alg, params in algs.items():
        start = time.time()
        res = part2(puzzle_input, filter_max=max_filter_size, blur_fun=params['fun'], transform=params['transform'])
        end = time.time()
        print("part2:{} [{}]\telapsed time: {}secs)".format(res, alg, end-start))


"""
part1:(241, 40) [numpy ]	elapsed time: 0.8465249538421631secs)
part1:(241, 40) [np_sep]	elapsed time: 0.3473501205444336secs)
part1:(241, 40) [torch ]	elapsed time: 0.3159973621368408secs)
part1:(241, 40) [torch_sep]	elapsed time: 0.24198198318481445secs)
part1:(241, 40) [opencv]	elapsed time: 0.14795207977294922secs)
part2:(166, 75, 12) [numpy ]	elapsed time: 85.33897590637207secs)
part2:(166, 75, 12) [np_sep]	elapsed time: 1.6929290294647217secs)
part2:(166, 75, 12) [torch ]	elapsed time: 298.3986382484436secs)
part2:(166, 75, 12) [torch_sep]	elapsed time: 4.763312101364136secs)
part2:(166, 75, 12) [opencv]	elapsed time: 0.2956364154815674secs)
"""
