import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats
import seaborn as sns
from sklearn.feature_selection import mutual_info_classif
from sklearn.model_selection import train_test_split
import warnings, argparse
warnings.filterwarnings(action='once')
from Transaction import Transaction


def histogram_intersection(a, b):
    return np.minimum(a, b).sum().round(decimals=1)

def getSeries(nr, transactions):
    result = list()
    if nr == 30:
        for t in transactions:
            result.append(t.status)
    else:
        for t in transactions:
            result.append(t.attributes[nr])
    return result

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--path', type = str, required = True, help = 'path to file with dataset')
    args = parser.parse_args()

    # transactions = list()

    # with open(args.path', 'r') as f:
    #     for line in f:
    #         splitted = line.rstrip('\n').split(',')
    #         if splitted[0] == '"Time"':
    #             continue

    #         attributes = list()
    #         for i in range(30):
    #             attributes.append(float(splitted[i]))

    #         status = int(splitted[30].strip('"'))

    #         t = Transaction(attributes, status)
    #         transactions.append(t)


    # #transactions[0].show()
    # #print(len(transactions))


    # # counter0 = 0  # "0" class occurences count
    # # counter1 = 0  # "1" class occurences count
    # # for t in transactions:
    # #     if t.status == 0:
    # #         counter0 += 1
    # #     else:
    # #         counter1 += 1

    # # print(counter0)
    # # print(counter1)


    # # corr research
    # # I
    # # pairs = list()

    # # statuses = pd.Series(getSeries(30, transactions))
    # # for i in range(1,29):
    # #     series = pd.Series(getSeries(i, transactions))
    # #     res = statuses.corr(series, method=histogram_intersection)
    # #     pairs.append((i,res))

    # # pairs.sort(key=lambda tup: tup[1])
    # # for pair in pairs:
    # #     print(f'{pair[0]} - {pair[1]}')

    # # II
    # statuses = pd.Series(getSeries(30, transactions))
    # for i in range(1,29):
    #     series = pd.Series(getSeries(i, transactions))
    #     data = mutual_info_classif(series, statuses, discrete_features=False, random_state=1)



    transactions = pd.read_csv(args.path)
    # print(transactions.shape)
    # print(transactions.isnull().any().any())
    # print(transactions.head())

    #print(transactions['Class'].value_counts())
    #print(transactions['Class'].value_counts(normalize = True))

    X = transactions.drop(labels = 'Class', axis = 1)  # Features
    y = transactions.loc[:, 'Class']                 # Response

    X_train , X_test, y_train, y_test = train_test_split(X , y , test_size=0.2 , random_state=1 , stratify = y)
    #print(X_train.shape)
    #print(X_test.shape)

    #to flag off warnings
    X_train.is_copy = False
    X_test.is_copy = False

    #print(X_train['Time'].describe())

    X_train.loc[:, 'Time'] = X_train.Time / 3600
    X_test.loc[:, 'Time'] = X_test.Time / 3600


    # plt.figure(figsize=(12,8))
    # sns.distplot(X_train['Time'],bins=50,color='green')
    # plt.xlim([0,50])
    # plt.xticks(np.arange(0,50,5))
    # plt.xlabel('Time after 1st transaction(hr)')
    # plt.ylabel('Count')
    # plt.title('Transaction times')
    # plt.show()


    #print(X_train['Amount'].describe())

    # plt.figure(figsize=(12,8), dpi=80)
    # sns.boxplot(X_train['Amount'])
    # plt.ylabel('Count')
    # plt.title('Transaction Amounts')
    # plt.show()

    # X_train['Amount'].skew()
    # X_train.loc[:,'Amount'] = X_train['Amount']+ 1e-9

    # X_train.loc[:,'Amount'], maxlog, (min_ci, max_ci) = sp.stats.boxcox(X_train['Amount'], alpha=0.01)


    pca_vars = ['V%i' % k for k in range(1,29)]

    # plt.figure(figsize=(12,8))
    # sns.barplot(x=pca_vars, y=X_train[pca_vars].mean(), color='green')
    # plt.xlabel('Column')
    # plt.ylabel('Mean')
    # plt.title('V1-V28 Means')
    # plt.show()

    # plt.figure(figsize=(12,8))
    # sns.barplot(x=pca_vars, y=X_train[pca_vars].std(), color='green')
    # plt.xlabel('Column')
    # plt.ylabel('Mean')
    # plt.title('V1-V28 Means')
    # plt.show()

    # plt.figure(figsize=(12,8))
    # sns.barplot(x=pca_vars, y=X_train[pca_vars].skew(), color='green')
    # plt.xlabel('Column')
    # plt.ylabel('Mean')
    # plt.title('V1-V28 Means')
    # plt.show()

    # plt.figure(figsize=(12,8))
    # sns.distplot(X_train['V8'], bins=100)
    # plt.ylabel('Count')
    # plt.title('V8')
    # plt.show()

    # plt.figure(figsize=(12,8), dpi=80)
    # sns.boxplot(X_train['V8'])
    # plt.ylabel('Count')
    # plt.title('V8')
    # plt.show()

    # plt.figure(figsize=(12,8))
    # plt.yscale('log')
    # sns.barplot(x=pca_vars, y=X_train[pca_vars].kurtosis(), color='green')
    # plt.xlabel('Column')
    # plt.ylabel('Kurtosis')
    # plt.title('V1-V28 Kurtoses')
    # plt.show()

    # plt.figure(figsize=(12,8))
    # sns.barplot(x=pca_vars, y=X_train[pca_vars].median(), color='green')
    # plt.xlabel('Column')
    # plt.ylabel('Mean')
    # plt.title('V1-V28 Means')
    # plt.show()

    # plt.figure(figsize=(12,8))
    # sns.barplot(x=pca_vars, y=X_train[pca_vars].quantile(0.75) - X_train[pca_vars].quantile(0.25), color='green')
    # plt.xlabel('Column')
    # plt.ylabel('IQR')
    # plt.title('V1-V28 IQRs')
    # plt.show()

    data = mutual_info_classif(X_train, y_train, discrete_features = False, random_state = 1)
    mutual_infos = pd.Series(data, index = X_train.columns)
    mutual_infos.sort_values(ascending = False)
    #print(mutual_infos)

    correlations_list = []
    attributes_list = []
    for attr, value in mutual_infos.items():
        correlations_list.append(value)
        attributes_list.append(attr)

    # correlations_list = mutual_infos.iloc[:,0:].values
    # attributes_list = mutual_infos.iloc[:,1:].values
    print(correlations_list)
    print()
    print(attributes_list)
    print()

    plt.figure(figsize=(36, 18))
    sns.barplot(x=attributes_list, y = correlations_list, color = 'green')
    plt.xlabel('Atrybuty', fontsize = 25)
    plt.ylabel('Wartość korelacji', fontsize = 25)
    plt.xticks(fontsize = 28, rotation = 45)
    plt.yticks(fontsize = 28)
    plt.title('Badanie korelacji', fontsize = 40)
    plt.savefig('corr_stats.png')

if __name__ == "__main__":
    exit(main())
