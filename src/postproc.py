
import os
import pandas as pd
import math

from settings import *

'''
We extract all the forecasts from the different data splits to compute the RMSE 

For instance
- 1973-2012 trainl; 2013-2022 test; 10 point forecasts
- 1973-2013 train; 2014-2022 test; 9 point forecasts
...
- 1973-2021 train; 2022 test; 1 point forecast

For each of these there is up to 10 point forecasts.
Reducing the number of forecasts by one as we increase the training set and reduce the testing set by 1.

We attach these point forecasts to sample-based mortality dataset.

i.e. we originally have
Age, 1973, 1974, ..., 2022
A0, <prob>, <prob>, ..., <prob>
A1, <prob>, <prob>, ..., <prob>
...
A100, <prob>, <prob>, ..., <prob>

And convert this to a sample-based format
y (probablity), Age, Year,
<prob>, A0, 1973
<prob>, A1, 1973
...
<prob>, A100, 1973
<prob>, A0, 1974
<prob>, A1, 1973
...
<prob>, A100, 1973
<prob>, A0, 2022
<prob>, A1, 2022
...
<prob>, A100, 2022

Which is further split into the train/test samples and input to the CFR.
There is an output file out/<seed>/<trainsplit>_<dataset>_<sm?> 
e.g. male smoothed: out/982897/1973_2012_male_sm    
e.g. male unsmoothed: out/982897/1973_2012_male

This produces the key outputs in the directory
<seed>.Run.log that contains the RMSE performance and the model
<seed>.Test.log that contains a copy of the test file
<seed>.Test.Predict.log that contains the predicted values of the model correlated to the rows in <seed>.Test.log
We can join these to have the data:
y, Age, Year, y'

We then take these predictions and construct the original data with the prediction data i.e.
y (probability), Age, Year, 1p forecast, 2p, ..., 10p

We note that
- We cannot include the model, as the 1p forecast, 2p forecaste etc. have different models.
- The models apply to the 1p, 2p, 3p, of different years, for example, the same model (m1) produces these forecasts
given the training dataset 1973-2012

y, Age, Year, forecast, produced by
<>, A0, 2013, 1p, m1
<>, A0, 2014, 2p, m1
...
<>, A0, 2022, 10p, m1

And m1 is used to predict for each different age in this manner.

We have a second model m2 that creates its own 1p, to 9p forecast for each age on the training dataset 1973-2013

y, Age, Year, forecast, produced by
<>, A0, 2014, 1p, m2
<>, A0, 2015, 2p, m2
...
<>, A0, 2022, 9p, m2

For simplicity, we construct the file as follows, each of the forecasts being constructed by a different model on a single line
y, Age, Year, 1p, 2p, ..., 10p

This allows to simply sum the 1p column where it exits to produce the average 1p forecast RMSE, etc.

'''
def post_proc():

    # For each seed executed
    for seed in seeds:

        # For exach experiement; male_sm, male, female_sm, female, etc.
        for data_group in datasets:

            # Get the sample-based format
            df = pd.read_csv(data_group[0])
            
            # Get all the subexperiments for each experiement 
            # This will be a model that is produced on each train/test split
            # e.g. for male_sm: [1973-2012, 1973-2013, ..., 1973-2021]
            resDir = f"{dirOut}/{seed}"
            subexps = [
                d
                for d in os.listdir(resDir)
                if d.endswith(data_group[1])
            ]
            
            # For each subexperiment
            for subexp in subexps:

                # Concatenate the test file and the predicted values
                subexpResDir = f"{resDir}/{subexp}"
                subexpResTestFile = f"{subexpResDir}/{seed}.Test.csv"
                subexpResPredFile = f"{subexpResDir}/{seed}.Test.Predict.csv"

                # Read the test and prediction files
                subexpDf = pd.read_csv(subexpResTestFile)
                predDf = pd.read_csv(subexpResPredFile).rename(columns={'y': 'yd'})  # Rename 'y' to 'yd' in predDf

                # Concatenate the test and predicted values
                expDf = pd.concat([subexpDf, predDf], axis=1)

                # Determine the offset in predictions, i.e. what i'th forecast is this sample?
                # This allows us to associate the correct column next.
                min_year = expDf['year'].min()
                expDf['offset'] = (expDf['year'] - min_year + 1)

                # Now we have the following format and can assign values against df
                # y, Age, Year, y', offset

                # Iterate over expDf rows and update df
                for _, row in expDf.iterrows():
                    age = row['age']
                    year = row['year']
                    offset = int(row['offset'])  # Ensure offset is an integer
                    predicted_value = row["yd"]  # Assuming the predicted column in expDf is named "yd"
                    
                    mask = (df['age'] == age) & (df['year'] == year)
                    actual_value = df.loc[mask, 'y'].values[0] if not df.loc[mask, 'y'].empty else None

                    squared_error = (actual_value - predicted_value) ** 2

                    # Locate the row in df with matching 'Age' and 'Year'
                    mask = (df['age'] == age) & (df['year'] == year)
                    df.loc[mask, f'{offset}p'] = predicted_value
                    df.loc[mask, f'{offset}p'] = predicted_value
                    df.loc[mask, f'{offset}se'] = squared_error

            output_file = f"{resDir}/{data_group[0].replace('data/','').replace('.csv',f'{data_group[1]}.csv')}"
            df.to_csv(output_file, index=False)

            # Initialize a list to store step averages
            step_averages = []

            # Compute step-RMSE values for each offset (1 to 10)
            for h in range(1, 11):  # For each offset
                se_col = f'{h}se'

                if se_col in df.columns:
                    # Filter rows with valid values in the {offset}se column
                    step_df = df[df[se_col].notna()]

                    # Compute the average value for the given step
                    avg_value = math.sqrt((1 / (101 * (11 - h))) * step_df[se_col].mean())

                    # Store the result in the step_averages list
                    step_averages.append({'h': h, 'average': avg_value})

            # Convert step averages to a DataFrame
            avg_df = pd.DataFrame(step_averages)

            # Save only the h and average columns to a file
            avg_output_file = f"{resDir}/{data_group[0].replace('data/', '').replace('.csv', f'{data_group[1]}_step.csv')}"
            avg_df.to_csv(avg_output_file, index=False)

post_proc()
