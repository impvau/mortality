
from settings import *
import pandas as pd
import os

''' Convert original mortality data to sample structure '''
def to_samples(source, destination):

    # Create folder structure to output file if necessary
    os.makedirs(os.path.dirname(destination), exist_ok=True)

    df = pd.read_csv(source)
    
    # Extract numeric age from the first column
    df['age'] = df.iloc[:, 0].str.extract(r'A(\d+)').astype(int)

    # Identify year columns
    year_columns = [col for col in df.columns if col.isdigit()]

    # Melt the DataFrame to reshape it from wide to long format
    long_df = df.melt(id_vars='age', value_vars=year_columns, var_name='year', value_name='y')

    # Convert year column to integer
    long_df['year'] = long_df['year'].astype(int)

    # Reorder columns to make 'y' the first column
    long_df = long_df[['y', 'age', 'year']]

    all_samples_df = long_df[(long_df['year'] >= 1973) & (long_df['year'] <= 2022)]
    all_samples_df.to_csv(destination, index=False)

''' Convert sample to train/test splits '''
def to_splits(source, fc_points):

    df = pd.read_csv(source)
    dirDest = os.path.dirname(source)

    # Create train and test files for each shift
    for shift in range(fc_points):

        train_start = initial_train_start
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

# Convert sources to sample files
for data_sources in get_sources():
    to_samples(data_sources["original"], data_sources["destination"])
    to_splits(data_sources["destination"], fc_points)
