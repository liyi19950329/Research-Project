# import numpy as np
#
# ######### how to work for one Neuron ##########
# # activate function
# def sigmoid(x):
#     return 1 / (1 + np.exp(-x))
#
#
# class Neuron:
#     def __init__(self,weights, bias):
#         self.weights = weights
#         self.bias = bias
#     def feedforward(self,inputs):
#         # Weight inputs, add bias, then use the activation function
#         total = np.dot(self.weights, inputs)
#         return sigmoid(total)
#
# # weights = np.array([0, 1]) # w1 = 0, w2 = 1
# # bias = 4                   # b = 4
# # n_test = Neuron(weights, bias)
# #
# # input = np.array([2, 3])            # x1 = 2, x2 = 3
# # print(n_test.feedforward(input))    # 0.9990889488055994
#
#
# ######### build neuronl network ##########
# class OurNeuralNetwork:
#   '''
#   A neural network with:
#     - 2 inputs
#     - a hidden layer with 2 neurons (h1, h2)
#     - an output layer with 1 neuron (o1)
#   Each neuron has the same weights and bias:
#     - w = [0, 1]
#     - b = 0
#   '''
#   def __init__(self):
#     weights = np.array([0, 1])
#     bias = 0
#
#     # The Neuron class here is from the previous section
#     self.h1 = Neuron(weights, bias)
#     self.h2 = Neuron(weights, bias)
#     self.o1 = Neuron(weights, bias)
#
#   def feedforward(self, x):
#     out_h1 = self.h1.feedforward(x)
#     out_h2 = self.h2.feedforward(x)
#
#     # The inputs for o1 are the outputs from h1 and h2
#     out_o1 = self.o1.feedforward(np.array([out_h1, out_h2]))
#
#     return out_o1
#
# network = OurNeuralNetwork()
# x = np.array([2, 3])
# print(network.feedforward(x)) # 0.7216325609518421

######### how to work for one hidden layer NN ##########
import numpy as np
import pandas as pd
import xlrd
from sklearn import preprocessing
import pdb

filename = '~/Desktop/RP/day_in_the_life.xls'

def sigmoid(x):
  # Sigmoid activation function: f(x) = 1 / (1 + e^(-x))
  return 1 / (1 + np.exp(-x))

def deriv_sigmoid(x):
  # Derivative of sigmoid: f'(x) = f(x) * (1 - f(x))
  fx = sigmoid(x)
  return fx * (1 - fx)

def mse_loss(y_true, y_pred):
  # y_true and y_pred are numpy arrays of the same length.
  return ((y_true - y_pred) ** 2).mean()

class OurNeuralNetwork:
  '''
  A neural network with:
    - 2 inputs
    - a hidden layer with 2 neurons (h1, h2)
    - an output layer with 1 neuron (o1)

  *** DISCLAIMER ***:
  The code below is intended to be simple and educational, NOT optimal.
  Real neural net code looks nothing like this. DO NOT use this code.
  Instead, read/run it to understand how this specific network works.
  '''
  def __init__(self):
    # Weights randomly produced
    self.w1 = np.random.normal()
    self.w2 = np.random.normal()
    self.w3 = np.random.normal()
    self.w4 = np.random.normal()
    self.w5 = np.random.normal()
    self.w6 = np.random.normal()

    # Biases randomly produced as well
    self.b1 = np.random.normal()
    self.b2 = np.random.normal()
    self.b3 = np.random.normal()

  def feedforward(self, x):
    # x is a numpy array with 2 elements.
    h1 = sigmoid(self.w1 * x[0] + self.w2 * x[1] + self.b1)
    h2 = sigmoid(self.w3 * x[0] + self.w4 * x[1] + self.b2)
    o1 = sigmoid(self.w5 * h1 + self.w6 * h2 + self.b3)
    print(h1, h2)
    return o1

  def train(self, data, all_y_trues):
    '''
    - data is a (n x 2) numpy array, n = # of samples in the dataset.
    - all_y_trues is a numpy array with n elements.
      Elements in all_y_trues correspond to those in data.
    '''
    learn_rate = 0.1
    epochs = 1000 # number of times to loop through the entire dataset

    for epoch in range(epochs):
      for x, y_true in zip(data, all_y_trues):
        # --- Do a feedforward (we'll need these values later)
        sum_h1 = self.w1 * x[0] + self.w2 * x[1] + self.b1
        h1 = sigmoid(sum_h1)

        sum_h2 = self.w3 * x[0] + self.w4 * x[1] + self.b2
        h2 = sigmoid(sum_h2)

        sum_o1 = self.w5 * h1 + self.w6 * h2 + self.b3
        o1 = sigmoid(sum_o1)
        y_pred = o1

        # --- Calculate partial derivatives.
        # --- Naming: d_L_d_w1 represents "partial L / partial w1"
        d_L_d_ypred = -2 * (y_true - y_pred)

        # Neuron o1
        d_ypred_d_w5 = h1 * deriv_sigmoid(sum_o1)
        d_ypred_d_w6 = h2 * deriv_sigmoid(sum_o1)
        d_ypred_d_b3 = deriv_sigmoid(sum_o1)

        d_ypred_d_h1 = self.w5 * deriv_sigmoid(sum_o1)
        d_ypred_d_h2 = self.w6 * deriv_sigmoid(sum_o1)

        # Neuron h1
        d_h1_d_w1 = x[0] * deriv_sigmoid(sum_h1)
        d_h1_d_w2 = x[1] * deriv_sigmoid(sum_h1)
        d_h1_d_b1 = deriv_sigmoid(sum_h1)

        # Neuron h2
        d_h2_d_w3 = x[0] * deriv_sigmoid(sum_h2)
        d_h2_d_w4 = x[1] * deriv_sigmoid(sum_h2)
        d_h2_d_b2 = deriv_sigmoid(sum_h2)

        # --- Update weights and biases
        # Neuron h1
        self.w1 -= learn_rate * d_L_d_ypred * d_ypred_d_h1 * d_h1_d_w1
        self.w2 -= learn_rate * d_L_d_ypred * d_ypred_d_h1 * d_h1_d_w2
        self.b1 -= learn_rate * d_L_d_ypred * d_ypred_d_h1 * d_h1_d_b1

        # Neuron h2
        self.w3 -= learn_rate * d_L_d_ypred * d_ypred_d_h2 * d_h2_d_w3
        self.w4 -= learn_rate * d_L_d_ypred * d_ypred_d_h2 * d_h2_d_w4
        self.b2 -= learn_rate * d_L_d_ypred * d_ypred_d_h2 * d_h2_d_b2

        # Neuron o1
        self.w5 -= learn_rate * d_L_d_ypred * d_ypred_d_w5
        self.w6 -= learn_rate * d_L_d_ypred * d_ypred_d_w6
        self.b3 -= learn_rate * d_L_d_ypred * d_ypred_d_b3

      # --- Calculate total loss at the end of each epoch
      if epoch % 10 == 0:
        # apply function on every piece data in array data
        y_preds = np.apply_along_axis(self.feedforward, 1, data)
        loss = mse_loss(all_y_trues, y_preds)
        print("Epoch %d loss: %.3f" % (epoch, loss))


def excel_to_matrix(path):
    table = xlrd.open_workbook(path).sheets()[0]#获取第一个sheet表
    row = table.nrows  # 行数
    col = table.ncols  # 列数
    datamatrix_feature_age = np.zeros((row, 1))#生成一个nrows行ncols列，且元素均为0的初始矩阵
    datamatrix_feature_gender = np.zeros((row, 1))  # 生成一个nrows行ncols列，且元素均为0的初始矩阵

    datamatrix_label = np.zeros((row, 1))#生成一个nrows行ncols列，且元素均为0的初始矩阵
    for x in range(col):
        # pdb.set_trace()
        cols = np.matrix(table.col_values(x))  # 把list转换为矩阵进行矩阵操作
        if x == 1:
            datamatrix_feature_age[:, 0] = cols # 按列把数据存进矩阵中
        elif x == 2:
          datamatrix_feature_gender[:, 0] = cols
        elif x == 3:
            datamatrix_label[:, 0] = cols
    # 数据归一化
    min_max_scaler = preprocessing.MinMaxScaler()
    datamatrix_feature_age = min_max_scaler.fit_transform(datamatrix_feature_age)
    datamatrix_label = min_max_scaler.fit_transform(datamatrix_label)
    datamatrix_feature = np.hstack((datamatrix_feature_age, datamatrix_feature_gender))
    print(datamatrix_label)
    return datamatrix_feature,datamatrix_label

datamatrix_feature,datamatrix_label = excel_to_matrix(filename)

# data_input = np.array(data[:,1],data[:,2])
print(datamatrix_feature,datamatrix_label)
# # Define dataset
# data = np.array([
#   [-2, -1],  # Alice
#   [25, 6],   # Bob
#   [17, 4],   # Charlie
#   [-15, -6], # Diana
# ])
# all_y_trues = np.array([
#   1, # Alice
#   0, # Bob
#   0, # Charlie
#   1, # Diana
# ])
#
# Train our neural network!
network = OurNeuralNetwork()
network.train(datamatrix_feature, datamatrix_label)



emily = np.array([65,2])
emily2 = np.array([0.9, 1])
emily3 = np.array([0.23, 2])


print("Emily: %.3f" % network.feedforward(emily))
print("Emily2: %.3f" % network.feedforward(emily2))
print("Emily3: %.3f" % network.feedforward(emily3))