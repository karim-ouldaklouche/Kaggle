# -*- coding: utf-8 -*-
"""
Created on Fri Oct 20 11:49:07 2017

@author: karim
"""

import pandas
train = pandas.read_csv('../data/train.csv')
X_test = pandas.read_csv('../data/test.csv')

X_train = train.iloc[:,1:] 
y_train = train.iloc[:,:1]

from keras.models import Sequential
from keras.layers import Dense, Flatten, Activation
from keras.layers import Conv2D, MaxPooling2D
from keras.optimizers import SGD
from keras.utils import np_utils

model = Sequential()

x_train = X_train.values.reshape(X_train.shape[0], 28, 28, 1)
x_test = X_test.values.reshape(X_test.shape[0], 28, 28, 1)
input_shape = (28, 28, 1)

print('input_shape:',input_shape)

kernel_size = (5,5)
input_shape = (28, 28, 1)
nb_filter = 32
pool_size = (2, 2)
pool = MaxPooling2D(pool_size=(pool_size))

model = Sequential()
model.add(Conv2D(32,kernel_size=(5,5),activation='sigmoid',input_shape=(28, 28, 1),padding='same'))
model.add(MaxPooling2D(pool_size=((2, 2))))
model.add(Conv2D(64,kernel_size=(5,5),activation='sigmoid',padding='same'))
model.add(MaxPooling2D(pool_size=((2, 2))))
model.add(Flatten())
model.add(Dense(100, activation='sigmoid', name='fc1'))
model.add(Dense(10, name='fc2'))
model.add(Activation('softmax'))

learning_rate = 0.5
sgd = SGD(learning_rate)
model.compile(loss='categorical_crossentropy',optimizer=sgd,metrics=['accuracy'])

batch_size = 300
nb_epoch = 20 

# convert class vectors to binary class matrices 4
Y_train = np_utils.to_categorical(y_train, 10)
model.fit(x_train, Y_train,batch_size=batch_size, epochs=nb_epoch,verbose=1)

scores = model.evaluate(x_train, Y_train, verbose=0)
print("%s: %.2f%%" % (model.metrics_names[0], scores[0]*100))
print("%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))

predictions = model.predict(x_test, batch_size=batch_size, verbose=1)

import pandas as pd
df = pd.DataFrame()
df_prediction = pd.DataFrame(predictions)

df['Label'] = df_prediction.idxmax(axis=1)
print(df)
#df.columns=['Label']
df.index+=1
df.index.name = 'Imageid'
df.to_csv('submission.csv', header=True)
