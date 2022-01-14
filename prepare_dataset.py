import os
from settings import PATHS

FILE_TO_READ = PATHS['dataset_path']+'creditcard.csv'
FILE_TO_WRITE = PATHS['dataset_path']+'creditcard_tmp.csv'

with open(FILE_TO_READ) as fRead:
    with open(FILE_TO_WRITE, 'w') as fWrite:
        for line in fRead:
            splitted = line.split(',')
            if splitted[0] == '"Time"':
                for i in range(0, len(splitted)):
                    splitted[i] = splitted[i].strip('"')
            else:
                #V1-V28 attributesround to 3 decimal places
                for i in range(1, 29):
                    splitted[i] = str(round(float(splitted[i]), 3))

                #class attribute convert to numeric
                splitted[30] = splitted[30].strip('"')

            transformedLine = ','.join(splitted)[:-2]
            fWrite.write(transformedLine + '\n')

os.remove(FILE_TO_READ)
os.rename(FILE_TO_WRITE, FILE_TO_READ)
