
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

for exp in experiments:

    # subExps are each of the test data splits
    subExps = get_experiements(exp[0])

    # Extract common elements from first record
    dfGround = pd.read_csv(subExps[0]["ground"])
    seed = subExps[0]["seed"]
    
    # Process results of each subexp and integrate against the dfGround
    for subExp in subExps:

        resDir = subExp["out"]

        # We have to merge the test file with the prediction file
        # due to the way the CFR splits the outputs
        testFile = f"{resDir}/{seed}.Test.csv"
        predFile = f"{resDir}/{seed}.Test.Predict.csv"
        testDf = pd.read_csv(testFile)
        predDf = pd.read_csv(predFile).rename(columns={'y': 'yd'})

        # Concatenate the test data and predicted values
        # now we have all info to lookup dfGround and associate
        # the particular forecast
        expDf = pd.concat([testDf, predDf], axis=1)

        # Determine the offset in predictions as for different
        # datasets the 1-point forecast is a different year as
        # the range of train and test increments for splits 1 to 10
        min_year = expDf['year'].min()
        expDf['offset'] = (expDf['year'] - min_year + 1)
        
        # Now we have the following format and can assign values against dfGround
        # y, Age, Year, yd, offset
        
        # Iterate over the results of this subExp to create prediction results
        for _, row in expDf.iterrows():

            # Extract key info
            age = row['age']
            year = row['year']
            offset = int(row['offset'])
            predicted_value = row["yd"]
            
            # Lookup the ground truth value. This could be raw or smoothed data
            # We generally only care about the raw data but some experiements compare
            # training and testing performance just on smoothed data
            mask = (dfGround['age'] == age) & (dfGround['year'] == year)
            actual_value = dfGround.loc[mask, 'y'].values[0] if not dfGround.loc[mask, 'y'].empty else None

            # Compute the squared error that is later averaged,
            # over the number of samples (1/(101*(11-h)))
            squared_error = (actual_value - predicted_value) ** 2

            # Locate the record in the ground truth with matching 'Age' and 'Year'
            mask = (dfGround['age'] == age) & (dfGround['year'] == year)

            # We set the predicted value for the ith forecast
            dfGround.loc[mask, f'{offset}p'] = predicted_value

            # We set the squared error we will later turn into RMSE
            dfGround.loc[mask, f'{offset}se'] = squared_error

        # Output cumulating file for verbose validation
        #output_file = f"{subExp['summary']}/summary_at_{min_year}.csv"
        #dfGround.to_csv(output_file, index=False)

    # Output interim file for validating
    output_file = f"{subExp['summary']}/summary.csv"
    dfGround.to_csv(output_file, index=False)

    # Compute averages for each step for the paper
    step_averages = []
    for h in range(1, fc_points+1):

        se_col = f'{h}se'
        if se_col in dfGround.columns:

            # Get rows that have SE errors
            step_df = dfGround[dfGround[se_col].notna()]

            # Compute the average value for the given step
            avg_value = math.sqrt((1 / (101 * (fc_points+1 - h))) * step_df[se_col].mean())

            # Store the result in the step_averages list
            step_averages.append({'h': h, 'average': avg_value})

    # Convert step averages to a DataFrame
    avg_df = pd.DataFrame(step_averages)

    # Save only the h and average columns to a file
    avg_df.to_csv(output_file.replace("summary","averages"), index=False)
