
import os

from settings import *

cmd = "src/memetico/bin/main -d 0.01 -dm stale-ext -dc 5 -dd adp-rnd -f 6 -g 200 -ld 0.2 -ls cnm -mr 0.2 -mt 3600 -p 18 -o rmse -cu"

dirOut = f"{dir}/out"

# Seeds to execute on. NB CFR is nondeterministic
seeds = ["1339061"]

# Configs to specify all dataset
prefs = ["Japan"]               
sexes = ["female","male","total"]
windows = ["all","last10"]
types = ["", "smooth"]              # "" = raw, 
variates = ["mv", "uv"]             # multivariate / univariate runs

# Overwrite with test datasets
prefs = ["Japan"]
sexes = ["female", "male"]
windows = ["all"]
types = [""]
variates = ["mv"]

for seed in seeds:
    for sex in sexes:
        for pref in prefs:
            for window in windows:
                for type in types:
                    for variate in variates:
                        for point in range(1,fc_points+1):

                            # Unique structure for both data files and unique output directories
                            dirStructure = f"{sex}/{pref}{'' if type == '' else '_' + type}/{window}/{variate}"

                            # Setup train
                            trainFile = f"{dirDataOut}/{dirStructure}/t{point}.csv"
                            testFile = f"{dirDataOut}/{dirStructure}/Te{point}.csv"
                            
                            # Setup output
                            dirExpOut = f"{dirOut}/{seed}/{dirStructure}/{point}"
                            os.makedirs(dirExpOut, exist_ok=True)

                            fullCmd = (
                                f'{cmd} '
                                f'-s {seed} '
                                f'-t {trainFile} '
                                f'-T {testFile} '
                                f'-lt {dirExpOut} '
                            )

                            print(fullCmd)
                            
                            os.system(fullCmd)
