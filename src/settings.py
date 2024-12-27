
import os

# Directory of the current file
dir = os.path.dirname(os.path.abspath(__file__))        

# Base directory that regression samples exist in for input to the SR methods
dirDataOut = f"{dir}/data"

# Number of forecast points
fc_points = 10
