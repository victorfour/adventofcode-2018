import numpy as np

def parse_file(filename):
    with open(filename) as f:
        res = [tuple(map(int, line.split(','))) for line in f]
    return np.array(res).T

def np_dist_matrix(grid, points):
    ax = grid[0,:]
    ay = grid[1,:]
    bx = points[0,:]
    by = points[1,:]
    res = np.abs(ax.reshape((1, -1)) - bx.reshape((1, -1)).T ) + np.abs(ay.reshape((1, -1)) - by.reshape((1, -1)).T )
    return res.T

data = parse_file("../../resources/day06/input.txt")
max_x = max(data[0,:]) + 1
max_y = max(data[1,:]) + 1


grid = np.array([(x,y) for x in range(max_x) for y in range(max_y)]).T

dist = np_dist_matrix(grid, data)

counts = np.sum(dist == np.min(dist, axis=1).reshape(len(dist), -1), axis=1)

argmin = np.argmin(dist, axis = 1)
reshaped_argmin = argmin.reshape((max_x, max_y))

top = reshaped_argmin[0, :]
bottom = reshaped_argmin[-1, :]
left = reshaped_argmin[:, 0]
right = reshaped_argmin[:, -1]
border = np.unique(np.concatenate([top, bottom, left, right]))

interior = argmin[(counts == 1) & np.isin(argmin, border, invert=True)]
freqs = np.unique(interior, return_counts=True)

res = np.max(freqs[1])
print("part1:", res)
