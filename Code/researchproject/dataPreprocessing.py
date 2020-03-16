import xlrd
import xlwt
import re
import pandas as pd
from pandas import DataFrame
from xlutils.copy import copy



filename = '~/Desktop/day_in_the_life 2 3.xls'


def merge_locationType():
    df = pd.read_excel(filename, index=None)

    df = df.groupby(['Subject', 'Age', 'Gender'])['Location Type'].apply(ab)
    df = df.reset_index()
    DataFrame(df).to_excel(filename, sheet_name='day_in_the_life', index=False, header=True)
def ab(df):
    return '-'.join(df.values)


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
    merge_locationType()