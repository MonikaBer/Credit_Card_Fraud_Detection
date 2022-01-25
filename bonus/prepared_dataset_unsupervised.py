import os, argparse
##removing classes with 1

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--path', type = str, required = True, help = 'path to file with dataset')
    args = parser.parse_args()

    with open(args.path) as fRead:
        with open(args.path[:-4] + '_unsupervised'+args.path[-4:], 'w') as fWrite:
            for line in fRead:
                splitted = line.split(',')
                if splitted[30] == "0\n" or splitted[30] =="Class\n":
                
                    transformedLine = ','.join(splitted)
                    fWrite.write(transformedLine)
		

if __name__ == '__main__':
    exit(main())
