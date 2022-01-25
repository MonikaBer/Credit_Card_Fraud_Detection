import os, argparse

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--path', type = str, required = True, help = 'path to file with dataset')
    args = parser.parse_args()

    with open(args.path) as fRead:
        with open(args.path + '_tmp', 'w') as fWrite:
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

    os.remove(args.path)
    os.rename(args.path + '_tmp', args.path)

if __name__ == '__main__':
    exit(main())