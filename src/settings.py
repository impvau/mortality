

dirData = "data"
dirOrig = f"{dirData}/orig"
dirOut = "out"

fc_points = 10

cmd = "src/memetico/bin/main -d 0 -dm stale-ext -dc 5 -dd adp-rnd -f 4 -g 10 -ld 0.2 -ls cnm -mr 0.2 -mt 600 -p 18 -o rmse"

seeds = ["982897"]


# Define file paths
file_paths = [
    ["data/Japan_National_smooth_male.csv", "data/jp_male_sm_t", "data/jp_male_sm_Te"],
    ["data/Japan_National_smooth_female.csv", "data/jp_female_sm_t", "data/jp_female_sm_Te"],
    ["data/Japan_National_male.csv", "data/jp_male_t", "data/jp_male_Te"],
    ["data/Japan_National_female.csv", "data/jp_female_t", "data/jp_female_Te"]
]

experiments = [
    # Name              Ground          Data for model
    ["jp_male_Mrr",     "jp_male",      "jp_male"],
    #["jp_male_Msr",     "jp_male_sm",   "jp_male"],
    #["jp_male_Mss",     "jp_male_sm",   "jp_male_sm"],
    #["jp_female_Mrr",   "jp_female",    "jp_female"],
    #["jp_female_Msr",   "jp_female_sm", "jp_female"],
    #["jp_female_Mss",   "jp_female_sm", "jp_female_sm"]
]

''' Get source files used in experiements and the pre-processed files '''
def get_sources():

    # Use a set to collect unique values
    unique_values = set()

    # Iterate through each row and add values from the second and third columns
    for experiment in experiments:
        unique_values.add((
            f"{dirOrig}/{experiment[1]}.csv",
            f"{dirData}/{experiment[1]}/all.csv"
        ))
        unique_values.add((
            f"{dirOrig}/{experiment[2]}.csv",
            f"{dirData}/{experiment[2]}/all.csv"
        ))

    # Convert the set of tuples to a list of dictionaries (if needed)
    unique_values = [{"original": src, "destination": dest} for src, dest in unique_values]

    return unique_values

''' Get all experiental configurations '''
def get_experiements():

    # We produce an array of all the data configurations that we need evaluate
    # We need to create a model for each differen train/test split, for instance
    # 1973-2012 ; 2013-2022
    # 1973-2013 ; 2014-2022
    # ...
    # 1973-2021 ; 2022
    
    # A model is produced on each of these splits, and we sum all the 1st point predictions
    # to assess how the overall method (say CFR) performs at 1st point forecasts
    # So in reality, the 1st point forecasts average the performance of the fc_points number
    # of models produced on the different split

    datas = []
    for seed in seeds:
        for experiment in experiments:
            for point in range(1,fc_points+1):

                datas.append({

                    "experiment": experiments[0],         
                    "fc_points": fc_points,
                    "seed": seed,
                    "point": point,

                    # Ground truth; this could be the original raw data or data that has been smoothed
                    # i.e. models can be trained on smooth data but assessed on raw
                    # or models can be trained on raw and assessed on raw
                    "ground": f"{dirData}/{experiment[1]}/all.csv",    

                    "source": f"{dirData}/{experiment[2]}/all.csv",    

                    # File to hold train data for a specific Year split
                    "train": f"{dirData}/{experiment[2]}/t{point}.csv",  

                    # File to hold test data for a specific Year split  
                    "test": f"{dirData}/{experiment[2]}/Te{point}.csv",

                    # Filte to store results in
                    "out": f"{dirOut}/{seed}/{experiment[0]}/{point}/"

                })

    return datas
