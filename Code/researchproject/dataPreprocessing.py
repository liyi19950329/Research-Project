from datetime import time
import datetime
import pandas as pd
from pandas import DataFrame
import pdb
from itertools import groupby
import re
import numpy as np
import xlrd
import xlwt
from math import isnan


filename = '~/Desktop/day_in_the_life_drop_suffix.xls'
filename_entire = '~/Desktop/day_in_the_life.xlsx'
duration_threshold = 60

def abbreviation(df):
    # df = pd.read_excel(filepath, index=None)
    # pdb.set_trace()
    for index, row in df.iterrows():
        drop = re.sub(u"\\(.*?\\)","",row['Location_Type'])
        strip = drop.strip()
        abbre = "".join([i[0].upper() for i in strip.split(" ")])
        df.loc[index, 'Location_Type'] = abbre
    # DataFrame(df).to_excel(filepath, sheet_name='day_in_the_life', index=False, header=True)
    return df

def move(df):
    # df = pd.read_excel(filepath, index=None)
    for index, row in df.iterrows():
        if row['Location_Type'] in ["PT","CJ"]:
            df.loc[index, 'Location_Type'] = "M"
    # DataFrame(df).to_excel(filepath, sheet_name='day_in_the_life', index=False, header=True)
    return df

def merge_locationType(df):
    # df = pd.read_excel(filepath, index=None)
    # pdb.set_trace()
    # for i in range(0, len(df)):
    #     if (df.iloc[i+1]['Subject'] == df.iloc[i]['Subject']) & (df.iloc[i]['Location_Type'] == df.iloc[i+1]['Location_Type']):
    #         df.loc[i, 'Duration'] = df.iloc[i+1]['Duration'] + df.iloc[i]['Duration']
    #         row = df.iloc[i + 1]
    #         df.drop(row, axis=0)
    row_del = []
    for index, row in df.iterrows():
        if (index > 0) & (df.iloc[index-1]['Subject'] == df.iloc[index]['Subject']) & (df.iloc[index]['Location_Type'] == df.iloc[index-1]['Location_Type']):
            df.loc[index, 'Duration'] = df.iloc[index-1]['Duration'] + df.iloc[index]['Duration']
            row_del.append(index-1)
    # pdb.set_trace()
    df = df.drop(row_del, axis=0)
    row_del_thres = []
    for index, row in df.iterrows():
        if row['Duration'] < duration_threshold:
            row_del_thres.append(index)
    df = df.drop(row_del_thres, axis=0)
    df = df.groupby(['Subject', 'Age', 'Gender'])['Location_Type'].apply(ab)
    df = df.reset_index()
    # DataFrame(df).to_excel(filepath, sheet_name='day_in_the_life', index=False, header=True)
    return df


def ab(df):

    locations = df.values
    locations_dupe = [x[0] for x in groupby(locations)]

    return '-'.join(locations_dupe)

def recalculate_age(df):
    # df = pd.read_excel(filename, index=None)
    for index, row in df.iterrows():
        if (row['Age'] >= 20) & (row['Age'] < 30):
            df.loc[index,'Age'] = "20-30"
        elif (row['Age'] < 20):
            df.loc[index,'Age'] = "0-20"
        elif (row['Age'] >= 30) & (row['Age'] < 40):
            df.loc[index,'Age'] = "30-40"
        elif (row['Age'] >= 40) & (row['Age'] < 50):
            df.loc[index,'Age'] = "40-50"
        elif (row['Age'] >= 50) & (row['Age'] < 60):
            df.loc[index,'Age'] = "50-60"
        elif (row['Age'] >= 60) & (row['Age'] < 70):
            df.loc[index,'Age'] = "60-70"
        elif (row['Age'] >= 70) & (row['Age'] < 80):
            df.loc[index,'Age'] = "70-80"
        else:
            df.loc[index,'Age'] = "80-100"
    print(df)
    DataFrame(df).to_excel(filename, sheet_name='day_in_the_life', index=False, header=True)

def difftime(df):
    # df = pd.read_excel(filename_entire, index=None)
    for index, row in df.iterrows():
        if pd.isnull(row['Time of Departure']) | pd.isnull(row['Time of Arrival']):
            df.loc[index,'Duration'] = 'NULL'
        elif pd.isnull(re.match("[0000]+",str(row['Time of Departure']))) & pd.isnull(re.match("[0000]+",str(row['Time of Arrival']))):
            df.loc[index, 'Duration'] = ((row['Time of Departure']-row['Time of Arrival']).seconds)/60
            # print(df.loc[index, 'Duration'])
        else:
            df.loc[index, 'Duration'] = 'NULL'
    DataFrame(df).to_excel(filename_entire, sheet_name='day_in_the_life', index=False, header=True)

def getTimeDiff(timeStra, timeStrb):
    if timeStra <= timeStrb:
        return 0
    ta = time.strftime(timeStra, "%Y/%m/%d %H:%M:%S %p")
    tb = time.strftime(timeStrb, "%Y/%m/%d %H:%M:%S %p")
    y, m, d, H, M, S = ta[0:6]
    dataTimea = datetime.datetime(y, m, d, H, M, S)
    y, m, d, H, M, S = tb[0:6]
    dataTimeb = datetime.datetime(y, m, d, H, M, S)
    secondsDiff = (dataTimea - dataTimeb).seconds
    minutesDiff = round(secondsDiff / 60, 1)
    return minutesDiff

# def replace():
#     file = open(filename1, 'r')
#     lines = file.readlines()
#     new_file = []
#     for i in range(len(lines)):
#         patterns = r'Private Transport\w*'
#         regex = re.compile(pattern = patterns)
#         subplaced_data = regex.sub('Private Transport', lines[i])
#         new_file.append(subplaced_data)
#
#         new_file_csvformat = []
#         for i in new_file:
#             new_line_split = i.split(',')
#             new_file_csvformat.append(new_line_split)
#
#     final_file = pd.DataFrame(new_file_csvformat)
#     final_file.to_csv('路径+新名字.csv', encoding='gbk', index=False, header=False)


# def merge_locationType():
#     df = pd.read_excel(file, index=None)
#     for i in range(1, len(df)):
#         if df.iloc[i]['Subject'] == df.iloc[i - 1]['Subject']:
#             trajectory = df.iloc[i - 1]['Location Type'] + "-" + df.iloc[i]['Location Type']
#             print(trajectory)
#             df.iloc[i - 1]['Location Type'] = trajectory
#             data = df.drop([i], axis=0)
#             DataFrame(data).to_excel(file, sheet_name='day_in_the_life', index=False, header=True)
#         else:
#             continue


# def merge_locationType():
#
#     day_in_the_life = xlrd.open_workbook(file)
#     sheet_day_in_life_xlrd = day_in_the_life.sheet_by_index(0)
#
#     # day_in_the_life_copy = copy(day_in_the_life)  # copy
#
#     # sheet_day_in_life_copy = day_in_the_life_copy.get_sheet(0)
#
#     nrows = sheet_day_in_life_xlrd.nrows
#
#     for i in range(1, nrows):
#         # trajectory = sheet_day_in_life_xlrd.cell(i, 5)
#         if sheet_day_in_life_xlrd.cell(i,0).value == sheet_day_in_life_xlrd.cell(i - 1,0).value:
#             trajectory = sheet_day_in_life_xlrd.cell(i - 1, 5).value + "-" + sheet_day_in_life_xlrd.cell(i, 5).value
#             sheet_day_in_life_xlrd.put_cell(i-1, 5, 1, trajectory, 0)
#
#             dic = {}
#             lis = []
#             for j in range(sheet_day_in_life_xlrd.ncols):
#                 lis.append(sheet_day_in_life_xlrd.cell(i, j).value)
#             dic[sheet_day_in_life_xlrd.cell(i, 0).value] = lis
#
#             # sheet_day_in_life_xlrd.rows(i).Delete()
#         else:
#             continue
#     # day_in_the_life_copy.save('day_in_the_life_afterProcessing.xls')
#
#     # sheet = pd.read_excel(file)
#     # print(sheet)
#     # count = sheet.ix[0][0]
#     # count += 1
#     # sheet.ix[0][0] = count
#
#     # print(sheet.ix[0][0])
#     # wb = xlrd.open_workbook(filename=file)  # 打开文件
#
#
#     # print(wb.sheet_names())  # get sheet names
#     #
#
#     #
#     # cols_locationType = day_in_the_life.col_values(5)  # column Location Type
#     #
#     # cols_subject = day_in_the_life.col_values(0)  # column Subject
#     #
#     # print(cols_locationType, cols_subject)
#
#     # sheet_day_in_the_life = wb.sheet_by_index(0)  # get sheet by index
#     #
#     #
#     # for i in range(1, nrows):
#     #     if sheet_day_in_the_life.cell(i,0).value == sheet_day_in_the_life.cell(i - 1,0).value:
#     #         trajectory = sheet_day_in_the_life.cell(i - 1, 5).value + "-" + sheet_day_in_the_life.cell(i, 5).value
#     #         sheet_day_in_the_life.write(i-1, 5, trajectory)
#     #         sheet_day_in_the_life.Rows(i).Delete()
#     #     else:
#     #         continue
#
#
#     #sheet2 = wb.sheet_by_name('day_in_the_life')  # get sheet by name
#
#     #print(sheet1, sheet2)
#
#     #print(sheet1.name, sheet1.nrows, sheet1.ncols)
#
#     #rows = sheet1.row_values(2)  # get row
#
#
#     # print(sheet1.cell(1, 0).value)  # three ways to get content in sheet
#     #
#     # print(sheet1.cell_value(1, 0))
#     #
#     # print(sheet1.row(1)[0].value)
#
#     def remove_order(self, num, dic4, dic2, bookname, sheetname):
#         flag = False
#         sum = 0
#         for i in dic4.keys():
#             if num == i and dic2[num][9] == '已退款':  # 判定指定属性，确定删除行
#                 sum += 1
#                 dic2.pop(num)
#                 self.updateExcle(dic2, 'D:\pythoncode\order.xls', 'Sheet1', sum)
#                 flag = True
#                 break
#         return flag


if __name__ == '__main__':
    # cwd = os.getcwd()  # Get the current working directory (cwd)
    # files = os.listdir(cwd)  # Get all the files in that directory
    # print("Files in %r: %s" % (cwd, files))
    # replace()
    # recalculate_age()
    # difftime()

    # read csv as dataframe
    df = pd.read_excel(filename_entire, index=None)

    # strip out content in parentheses and get first character to represent the word
    df_abb = abbreviation(df)

    # make all on the way location type as M
    df_move = move(df_abb)

    # merge all the location type for one subject
    df_merge = merge_locationType(df_move)

    # df_final = df_merge.groupby(['Location_Type'])





























