
import os
import shutil

cmd = "src/memetico/bin/main -d 0 -dm stale-ext -dc 5 -dd adp-rnd -f 4 -g 200 -ld 0.2 -ls cnm -mr 0.2 -mt 600 -p 18 -o rmse"

seeds = ["982897"]

dirIn = "data"
dirOut = "out"

datas = [
     ["1973_2012_male_sm","jp_male_sm_t1.csv","jp_male_sm_Te1.csv"],
    # ["1973_2013_male_sm","jp_male_sm_t2.csv","jp_male_sm_Te2.csv"],
    # ["1973_2014_male_sm","jp_male_sm_t3.csv","jp_male_sm_Te3.csv"],
    # ["1973_2015_male_sm","jp_male_sm_t4.csv","jp_male_sm_Te4.csv"],
    # ["1973_2016_male_sm","jp_male_sm_t5.csv","jp_male_sm_Te5.csv"],
    # ["1973_2017_male_sm","jp_male_sm_t6.csv","jp_male_sm_Te6.csv"],
    # ["1973_2018_male_sm","jp_male_sm_t7.csv","jp_male_sm_Te7.csv"],
    # ["1973_2019_male_sm","jp_male_sm_t8.csv","jp_male_sm_Te8.csv"],
    # ["1973_2020_male_sm","jp_male_sm_t9.csv","jp_male_sm_Te9.csv"],
    # ["1973_2021_male_sm","jp_male_sm_t10.csv","jp_male_sm_Te10.csv"],

    # ["1973_2012_female_sm","jp_female_sm_t1.csv","jp_female_sm_Te1.csv"],
    # ["1973_2013_female_sm","jp_female_sm_t2.csv","jp_female_sm_Te2.csv"],
    # ["1973_2014_female_sm","jp_female_sm_t3.csv","jp_female_sm_Te3.csv"],
    # ["1973_2015_female_sm","jp_female_sm_t4.csv","jp_female_sm_Te4.csv"],
    # ["1973_2016_female_sm","jp_female_sm_t5.csv","jp_female_sm_Te5.csv"],
    # ["1973_2017_female_sm","jp_female_sm_t6.csv","jp_female_sm_Te6.csv"],
    # ["1973_2018_female_sm","jp_female_sm_t7.csv","jp_female_sm_Te7.csv"],
    # ["1973_2019_female_sm","jp_female_sm_t8.csv","jp_female_sm_Te8.csv"],
    # ["1973_2020_female_sm","jp_female_sm_t9.csv","jp_female_sm_Te9.csv"],
    # ["1973_2021_female_sm","jp_female_sm_t10.csv","jp_female_sm_Te10.csv"]

    # ["1973_2012_male","jp_male_t1.csv","jp_male_Te1.csv"],
    # ["1973_2013_male","jp_male_t2.csv","jp_male_Te2.csv"],
    # ["1973_2014_male","jp_male_t3.csv","jp_male_Te3.csv"],
    # ["1973_2015_male","jp_male_t4.csv","jp_male_Te4.csv"],
    # ["1973_2016_male","jp_male_t5.csv","jp_male_Te5.csv"],
    # ["1973_2017_male","jp_male_t6.csv","jp_male_Te6.csv"],
    # ["1973_2018_male","jp_male_t7.csv","jp_male_Te7.csv"],
    # ["1973_2019_male","jp_male_t8.csv","jp_male_Te8.csv"],
    # ["1973_2020_male","jp_male_t9.csv","jp_male_Te9.csv"],
    # ["1973_2021_male","jp_male_t10.csv","jp_male_Te10.csv"],

    # ["1973_2012_female","jp_female_t1.csv","jp_female_Te1.csv"],
    # ["1973_2013_female","jp_female_t2.csv","jp_female_Te2.csv"],
    # ["1973_2014_female","jp_female_t3.csv","jp_female_Te3.csv"],
    # ["1973_2015_female","jp_female_t4.csv","jp_female_Te4.csv"],
    # ["1973_2016_female","jp_female_t5.csv","jp_female_Te5.csv"],
    # ["1973_2017_female","jp_female_t6.csv","jp_female_Te6.csv"],
    # ["1973_2018_female","jp_female_t7.csv","jp_female_Te7.csv"],
    # ["1973_2019_female","jp_female_t8.csv","jp_female_Te8.csv"],
    # ["1973_2020_female","jp_female_t9.csv","jp_female_Te9.csv"],
    # ["1973_2021_female","jp_female_t10.csv","jp_female_Te10.csv"]

]

def run():

    for seed in seeds:

        for data in datas:

            name = data[0]
            train_data = data[1]
            test_data = data[2]

            resDir = f"{dirOut}/{seed}/{name}"
            os.makedirs(resDir, exist_ok=True)
            
            fullCmd = f"{cmd} -s {seed} -t {dirIn}/{train_data} -T {dirIn}/{test_data} -lt {resDir}"
            #print(fullCmd)

            os.system(fullCmd)

run()
