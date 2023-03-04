#!/usr/bin/env python3
# Set the starting point
start = 4
n = 10
# Define the function that returns the ratio for each iteration
def get_ratio(i):
    if i <= 2:
        return 1.5
    elif i <= 4:
        return 2
    else:
        return 2.5
# Use a loop to calculate the next value in the progression at each iteration
for i in range(1, n+1):
    grid_points = start * get_ratio(i) 
    print("Iteration", i, ":", grid_points, "points")
