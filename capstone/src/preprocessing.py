""" Module with functions to preprocess the raw data. """
import datetime as dt
import os
import re
import pandas as pd


DATA_PATH = '../data/'
PROJECT_ROOT = os.getenv('BAYESIAN_ROOT', '/home/miguel/coursera/bayesian_statistics_2')
SRC = PROJECT_ROOT + 'src/'
EXCHANGE_RATES_PREFIX = 'currencies'
EXCHANGE_CSV_NAME = 'exchange_rates.csv'
DATE = 'date'

MONTHS = {'Jan': 1,
          'Feb': 2,
          'Mar': 3,
          'Apr': 4,
          'May': 5,
          'Jun': 6,
          'Jul': 7,
          'Aug': 8,
          'Sep': 9,
          'Oct': 10,
          'Nov': 11,
          'Dec': 12}


def parse_date(date_string):
    m_str, y_str = date_string.split()
    month = MONTHS[m_str]
    year = int(y_str)
    return dt.datetime(year, month, 15)


def parse__csv_file(file_path):
    data_df = pd.read_csv(file_path, header=1, skipfooter=1)
    data_df[DATE] = data_df.iloc[:,0].apply(parse_date)
    return data_df.set_index(DATE).drop(data_df.columns[0], axis=1)


def get_exchange_rate_csv(data_path=DATA_PATH, save_data=True):
    pattern = re.compile(EXCHANGE_RATES_PREFIX + '\d.csv')
    exchange_filenames = sorted([pattern.match(name).group(0) for name in os.listdir(data_path) if pattern.match(name)
                                 is not None])
    df_list = []
    for name in exchange_filenames:
        df_list.append(parse__csv_file(DATA_PATH + name))
    res = pd.concat(df_list, axis=1)
    if save_data:
        res.to_csv(DATA_PATH + EXCHANGE_CSV_NAME)
    return res

