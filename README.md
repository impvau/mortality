
# Mortality Forecasting with Continued Fraction Regression

# Usage

The code incldues
- CFR code (C++)
- Benchmarking code (R)
- Preprocessing/post-processing code (Python)

Execution and full reproduction can be achieved with
- Downloading repo with manual installation of expected packages
- Downloading repo and building dev containers for VS Code
- Downloading pre-build container and hitting execute

## Benchmarking Code

Code exists in `src/benchmark/point_forecast.R`

Dir of the execution full filepath is required which defaults to 
`dir <- "/workspaces/mortality/src/benchmark"`

Execute with 
- Rscript point_forecast.R
- load R, then `source('point_forecast.R')`
- Run VS Code Launch configuration for 'Point Forecasts'

On first execution
- Downloads all data to `src/benchmarking/cache/` (subsequent runs load from file)
- Loads all data and performs preprocessing into `prefectures` and `prefectures_smooth` objects (subsequent runs load .Rdata file)
- Runs all benchmarking methods and outputs point forecast tables


