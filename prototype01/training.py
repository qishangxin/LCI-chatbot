# coding=utf-8

import json
import numpy as np
import pickle
import random
import nltk
from HanTa import HanoverTagger as ht
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Activation, Dropout
from tensorflow.keras.optimizers import SGD

hannover = ht.HanoverTagger('morphmodel_ger.pgz')

intents = json.loads(open('intents.json', encoding='utf-8').read())

words = []
classes = []
documents = []
ignored_symbols = ['?', '!', '.', ',']

for intent in intents['intents']:
    for pattern in intent['patterns']:
        word_list = nltk.word_tokenize(pattern)
        words.extend(word_list)
        documents.append((word_list, intent['tag']))
        if intent['tag'] not in classes:
            classes.append(intent['tag'])

print(documents)

words = [str(hannover.analyze(word)[0]) for word in words if word not in ignored_symbols]
words = sorted(set(words))

print(words)

classes = sorted(set(classes))

pickle.dump(words, open('words.pkl', 'wb'))
pickle.dump(classes, open('classes.pkl', 'wb'))

training = []
output_empty = [0] * len(classes)

for document in documents:
    bag = []
    word_patterns = document[0]
    word_patterns = [str(hannover.analyze(word.lower())[0]) for word in word_patterns]
    for word in words:
        if word in word_patterns:
            bag.append(1)
        else:
            bag.append(0)

    output_row = list(output_empty)
    output_row = list(output_empty)
    output_row[classes.index(document[1])] = 1
    training.append([bag, output_row])

random.shuffle(training)
training = np.array(training)

training_x = list(training[:, 0])
training_y = list(training[:, 1])

model = Sequential()
model.add(Dense(128, input_shape=(len(training_x[0]),), activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(64, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(len(training_y[0]), activation='softmax'))

sgd = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
model.compile(loss='categorical_crossentropy', optimizer=sgd, metrics=['accuracy'])

hist = model.fit(np.array(training_x), np.array(training_y), epochs=300, batch_size=5, verbose=1)
model.save('chatbotmodel.h5', hist)

print("done")