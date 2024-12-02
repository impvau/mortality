import pandas as pd

# Define file paths
file_paths = [
    "data/Japan_National_smooth_male.csv",
    # "data/Japan_National_smooth_female.csv"
]

# Base output file names
output_train_base = "data/jp_male_sm_t"
output_test_base = "data/jp_male_sm_Te"

# Year ranges
initial_train_start = 1973
initial_train_end = 2012
initial_test_start = 2013
initial_test_end = 2022
shifts = 10

# Loop through the files
for file_path in file_paths:
    try:
        df = pd.read_csv(file_path)
        print(f"Processing file: {file_path}")

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

        # Create train and test files for each shift
        for shift in range(shifts):
            train_start = initial_train_start
            train_end = initial_train_end + shift
            test_start = train_end + 1
            test_end = initial_test_end

            # Filter data for train and test ranges
            train_df = long_df[(long_df['year'] >= train_start) & (long_df['year'] <= train_end)]
            test_df = long_df[(long_df['year'] >= test_start) & (long_df['year'] <= test_end)]

            # Save the train and test files
            train_output_file = f"{output_train_base}{shift + 1}.csv"
            test_output_file = f"{output_test_base}{shift + 1}.csv"

            train_df.to_csv(train_output_file, index=False)
            test_df.to_csv(test_output_file, index=False)

            print(f"Train file saved to: {train_output_file} (Years: {train_start}-{train_end})")
            print(f"Test file saved to: {test_output_file} (Years: {test_start}-{test_end})")
        
    except FileNotFoundError:
        print(f"File not found: {file_path}")
    except Exception as e:
        print(f"An error occurred while processing {file_path}: {e}")
