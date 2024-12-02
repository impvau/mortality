
import os
import shutil

cmd = "src/memetico/bin/main -d 0 -f 4 -g 200 -ld 0.2 -ls cnm -mr 0.2 -mt 600 -p 18 -o rmse"
seeds = ["982897"]

dirIn = "data"
dirOut = "out"

datas = [
    ["1973-2012","jp_male_sm_t1.csv","jp_male_sm_Te1.csv"],
    ["1973-2013","jp_male_sm_t2.csv","jp_male_sm_Te2.csv"],
    ["1973-2014","jp_male_sm_t3.csv","jp_male_sm_Te3.csv"],
    ["1973-2015","jp_male_sm_t4.csv","jp_male_sm_Te4.csv"],
    ["1973-2016","jp_male_sm_t5.csv","jp_male_sm_Te5.csv"],
    ["1973-2017","jp_male_sm_t6.csv","jp_male_sm_Te6.csv"],
    ["1973-2018","jp_male_sm_t7.csv","jp_male_sm_Te7.csv"],
    ["1973-2019","jp_male_sm_t8.csv","jp_male_sm_Te8.csv"],
    ["1973-2020","jp_male_sm_t9.csv","jp_male_sm_Te9.csv"],
    ["1973-2021","jp_male_sm_t10.csv","jp_male_sm_Te10.csv"]
]

def run():

    for seed in seeds:

        for data in datas:

            name = data[0]
            train_data = data[1]
            test_data = data[2]

            resDir = f"{dirOut}/{seed}/{name}"
            shutil.copyfile( f"{dirIn}/{train_data}", f"{resDir}/{train_data}")
            shutil.copyfile( f"{dirIn}/{test_data}", f"{resDir}/{test_data}")
            
            os.makedirs(resDir, exist_ok=True)
            fullCmd = f"{cmd} -s {seed} -t {dirIn}/{train_data} -T {dirIn}/{test_data} -lt {resDir}"
            print(fullCmd)

            os.system(fullCmd)

run()
