
######################################################################
#
# Pre-process Japanese mortality data into regression samples
#
# - Univariate (uv) is a CSV of the form y,year
# - Multivariate (mv) is a CSV of the form y,age,year
# - Makes a folder structure in src/data/<female|male|total>/<pref>|<pref>_smooth>/<all|last10>/<mv|uv>/
#   - folders have all.csv which is all samples between the filtered dates.
#     for all this is initial_train_start to initial_train_end defined in settings
#     for last10 this is initial_train_start_last10 to initial_train_end defined in settings
#     effecitvely for our experiements this is 1973 -> 2022 and 2002 -> 2022 respectively
#
# - The all.csv in each leaf directory is further split into the fc_point number of steps, for us being 10
#   This is 10 train/test splits i.e. for all data points
#   1973 -> 2012 ; 2013 -> 2022
#   1973 -> 2013 ; 2014 -> 2022
#   ...
#   1973 -> 2021 ; 2022
# 
#   For the last 10
#   2002 -> 2012 ; 2013 -> 2022
#   2003 -> 2013 ; 2014 -> 2022
#   ...
#   2012 -> 2021 ; 2022
#
######################################################################

from settings import *
import pandas as pd
import os

# Preproc-specific settings
dirDataOrig = f"{dir}/benchmark/data"

# Initial point ranges for filtering file data
initial_train_start = 1973
initial_train_end = 2012
initial_test_end = 2022
initial_train_start_last10 = initial_train_end-10

''' Convert sample file to a train/test split of fc_points length '''
def to_splits(source, fc_points = fc_points, train_start = initial_train_start):

    df = pd.read_csv(source)
    dirDest = os.path.dirname(source)

    # Create train and test files for each shift
    for shift in range(fc_points):

        train_end = initial_train_end + shift
        test_start = train_end + 1
        test_end = initial_test_end

        # Filter data for train and test ranges
        train_df = df[(df['year'] >= train_start) & (df['year'] <= train_end)]
        test_df = df[(df['year'] >= test_start) & (df['year'] <= test_end)]

        # Save the train and test files
        train_output_file = f"{dirDest}/t{shift + 1}.csv"
        test_output_file = f"{dirDest}/Te{shift + 1}.csv"

        train_df.to_csv(train_output_file, index=False)
        test_df.to_csv(test_output_file, index=False)

''' Convert original mortality data to sample structure for regression '''
def to_samples_and_splits(sourceFile, destination):

    # Create folder structure to output file if necessary
    os.makedirs(os.path.dirname(f"{destination}/all/mv/"), exist_ok=True)
    os.makedirs(os.path.dirname(f"{destination}/all/uv/"), exist_ok=True)
    os.makedirs(os.path.dirname(f"{destination}/last10/mv/"), exist_ok=True)
    os.makedirs(os.path.dirname(f"{destination}/last10/uv/"), exist_ok=True)

    df = pd.read_csv(sourceFile)
    
    # Extract numeric age from the first column
    df['age'] = df.iloc[:, 0].str.extract(r'A(\d+)').astype(int)

    # Shift the age by 1 to ensure there's no zero
    df['age'] = df['age'] + 1

    # Identify year columns
    year_columns = [col for col in df.columns if col.isdigit()]

    # Melt the DataFrame to reshape it from wide to long format
    long_df = df.melt(id_vars='age', value_vars=year_columns, var_name='year', value_name='y')

    # Convert year column to integer
    long_df['year'] = long_df['year'].astype(int)

    # Reorder columns to make 'y' the first column
    long_df = long_df[['y', 'age', 'year']]

    all_samples_df = long_df[(long_df['year'] >= initial_train_start) & (long_df['year'] <= initial_test_end)]
    all_samples_uni = all_samples_df[['y', 'year']]
    all_samples_multi = all_samples_df[['y', 'age', 'year']]
    all_samples_uni.to_csv(f"{destination}/all/uv/all.csv", index=False)
    to_splits(f"{destination}/all/uv/all.csv", fc_points, initial_train_start)
    all_samples_multi.to_csv(f"{destination}/all/mv/all.csv", index=False)
    to_splits(f"{destination}/all/mv/all.csv", fc_points, initial_train_start)

    last10_samples_df = long_df[(long_df['year'] >= initial_train_start_last10) & (long_df['year'] <= initial_test_end)]
    last10_samples_uni = last10_samples_df[['y', 'year']]
    last10_samples_multi = last10_samples_df[['y', 'age', 'year']]
    last10_samples_uni.to_csv(f"{destination}/last10/uv/all.csv", index=False)
    to_splits(f"{destination}/last10/uv/all.csv", fc_points, initial_train_start)
    last10_samples_multi.to_csv(f"{destination}/last10/mv/all.csv", index=False)
    to_splits(f"{destination}/last10/mv/all.csv", fc_points, initial_train_start)

for subdir in os.listdir(dirDataOrig):

    # Loop through data dirs; male, female, total
    subdir_path = os.path.join(dirDataOrig, subdir)
    if os.path.isdir(subdir_path):

        # Loop through files in each dir
        for filename in os.listdir(subdir_path):

            # For those that are files
            file_path = os.path.join(subdir_path, filename)
            if os.path.isfile(file_path):

                prefecture_dir = f"{dirDataOut}/{subdir}/{filename.replace('.csv','').replace('_female','').replace('_male','')}"
                to_samples_and_splits(file_path, prefecture_dir)
                