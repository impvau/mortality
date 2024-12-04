
import os

from settings import *

def run():

    for experiment in get_experiements():

        os.makedirs(experiment["out"], exist_ok=True)
        
        fullCmd = (
            f'{cmd} '
            f'-s {experiment["seed"]} '
            f'-t {experiment["train"]} '
            f'-T {experiment["test"]} '
            f'-lt {experiment["out"]} '
        )

        os.system(fullCmd)

run()
