import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import datetime
import urllib
import json
import csv
from collections import defaultdict
import csv, itertools, json

def lobbyists_count(df, year):
    #2013 lobbyist count
    df_year = df[(df.registration_year == year)][['lobbying_firm', 'lobbyist_last_name']].groupby(['lobbying_firm'], as_index=False)
    df_year_count = df_year.aggregate(np.count_nonzero)
    #Renaming fields to output to tree map file
    df_year_count.columns = ['name','size']
    return df_year_count

def dict_to_json(data):
    d = [ 
        dict([
            (colname, row[i]) 
            for i,colname in enumerate(data.columns)
        ])
        for row in data.values
    ]
    return d

def cluster(rows):
    result = []
    data = sorted(rows, key=lambda r: r[1])
    for k, g in itertools.groupby(rows, lambda r: r[0]):
        group_rows = [row[1:] for row in g]

        if len(row[1:]) == 1:
               result.append({"name": row[0],"size": (row[1])})
        else:
               result.append({"name": k,"children": cluster(group_rows)})
    return result

## download opendata from Socrata using SODA language
## topic - LA City Open Data Lobbyists 

def main():
    query_reglob = ('http://data.lacity.org/resource/j4zm-9kqu.json')
    df_reglob = pd.read_json(query_reglob)

    query_clientslob = ('http://data.lacity.org/resource/9z5d-hgrh.json')
    df_clientslob = pd.read_json(query_clientslob)

    query_payclient = ('http://data.lacity.org/resource/h6ky-vznd.json')
    df_payclient = pd.read_json(query_payclient)

    query_payproject = ('http://data.lacity.org/resource/4xuf-944h.json')
    df_payproject = pd.read_json(query_payproject)

    # data overview 
    df_reglob.dtypes
    df_clientslob.dtypes
    df_payclient.dtypes
    df_payproject.dtypes

    df_reglob.head()
    df_clientslob.head()
    df_payproject.head()

    df_reglob.describe()
    df_clientslob.describe()
    df_payproject.describe()

    # Data Analysis
    # Unused code commented out 
    # df_reglob['lobbying_firm'].unique()
    # firm_lob_count = df_reglob['lobbying_firm'].value_counts()
    # df_reglob[['lobbying_firm', 'registration_year','lobbyist_website']]
    # df_payclient.groupby(['lobbying_firm'], as_index=False).sum()

    #lobbyist count by year, will build a loop if there are more than 2 years
    firm_count_2013 = lobbyists_count(df_reglob, 2013)
    firm_count_2014 = lobbyists_count(df_reglob, 2014)

    #Convert dictionary to json
    d13 = dict_to_json(firm_count_2013)
    d14 = dict_to_json(firm_count_2014)

    #Print json to check data 
    print json.dumps(d13, indent=4, sort_keys=True)
    print json.dumps(d14, indent=4, sort_keys=True)

    #output json to file 
    # with open('firm_lobbyist_count_2013.json', 'w') as outfile:
    #   json.dump(d13, outfile, indent=4)

    # with open('firm_lobbyist_count_2014.json', 'w') as outfile:
    #   json.dump(d14, outfile, indent=4)

    #LA City client data 
    all_clients = df_clientslob['client_last_name'].unique()
    groupby_clients = df_clientslob[(df_clientslob.registration_year == 2014)][['client_last_name', 'client_state','client_email']].groupby(['client_last_name'], as_index=False)

    with open('LA_clients_pay.csv', 'wb') as myfile:
        wr = read_csv.writer(myfile, quoting=csv.QUOTE_ALL)
        wr.writerow(df_payclient)

    #Output file of unique clients to manually review
    # with open('LA_clients_unqiue.csv', 'wb') as myfile:
    #     wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
    #     wr.writerow(unique_client)

    #Create data to show clusters of lobbying firms, categories with amount paid sum
    df_pay = pd.read_csv('clientpaycat.csv') 
    a = df_pay.groupby('category')
    groupby_cat = df_pay[(df_pay.report_year == 2014)][['category', 'client_last_name','lobbying_firm','amount_paid']].groupby(['category'], as_index=False)
    df_cat_paid = groupby_cat.sum()
    plt.show()

    groupby_cat_firm = df_pay[(df_pay.report_year == 2014)][['category', 'lobbying_firm','amount_paid']].groupby(['category', 'lobbying_firm'], as_index=False)
    df_cat_firm = groupby_cat_firm.sum()

    groupby_cat.sort(['amount_paid'], ascending=False)[:3].boxplot(column='lobbying_firmpaid', by='category', grid=False), ascending=False)[:15].boxplot(col)

    groupby_firm = df_pay[(df_pay.report_year == 2014)][['category', 'lobbying_firm','client_last_name','amount_paid']].groupby(['lobbying_firm', 'category'], as_index=False)
    df_firmpaid = groupby_firm.aggregate(np.sum)
    df_firmpaid.sort(['amount_paid'], ascending=False).plot(x='lobbying_firm',y='amount_paid', kind='bar', stacked=True)
    df_firmpaid['amount_paid'].hist(by=df_firmpaid['amoount_paid'])

    #create csv to convert to json
    df_cat_firm.to_csv('LA_grp_cat_firm.csv')

    #output json to file 
    with open('firm_cat.json', 'w') as outfile:
        json.dump(d13, outfile, indent=4)

    #panadas to nested json
    results = defaultdict(lambda: defaultdict(dict))

    for index, value in df_firmpaid.itertuples():
        for i, key in enumerate(index):
            if i == 0:
                nested = results[key]
            elif i == len(index) - 1:
                nested[key] = value
            else:
                nested = nested[key]
    
    print json.dumps(results, indent=4)

    #create json from csv
    with open('LA_grp_cat_firm_2.csv', 'rb') as f:
        reader = csv.reader(f, quoting=csv.QUOTE_NONE)
        s = list(reader)

    rows = list(csv.reader(s.splitlines()))
    print json.dumps(cluster(s),indent=2)


if __name__ == '__main__':
    main() 