# coding=utf-8

import json
import numpy as np
import pickle
import random
import nltk
import subprocess
import pathlib
from HanTa import HanoverTagger as ht
from tensorflow.keras.models import load_model
from flask import Flask, render_template, request

LCI_FILE_NAME = "saints.json"

hannover = ht.HanoverTagger('morphmodel_ger.pgz')
intents = json.loads(open('intents.json', encoding='utf-8').read())

words = pickle.load(open('words.pkl', 'rb'))
classes = pickle.load(open('classes.pkl', 'rb'))
model = load_model('chatbotmodel.h5')

last_input_nouns = []
feedback_mode = False

def clean_up_sentence(sentence):
    sentence_words = nltk.word_tokenize(sentence)
    sentence_words = [str(hannover.analyze(word)[0]) for word in sentence_words]
    return sentence_words

def bag_of_words(sentence):
    sentence_words = clean_up_sentence(sentence)
    bag = [0] * len(words)
    for w in sentence_words:
        for i, word in enumerate(words):
            if word == w:
                bag[i] = 1
    return np.array(bag)

def predict_class(sentence):
    bow = bag_of_words(sentence)
    res = model.predict(np.array([bow]))[0]
    ERROR_THRESHOLD = 0.95
    results = [[i, r] for i, r in enumerate(res) if r > ERROR_THRESHOLD]

    results.sort(key=lambda x: x[1], reverse=True)
    return_list = []
    for r in results:
        return_list.append({'intent': classes[r[0]], 'probability': str(r[1])})
    return return_list

def get_response(intents_list, intents_json):
    tag = intents_list[0]['intent']
    list_of_intents = intents_json['intents']
    for i in list_of_intents:
        if i['tag'] == tag:
            result = random.choice(i['responses'])
            break
    return result

print("bot is running...")

def call_LCISearch(attribute_list):
    LCI_SEARCH_STRING = "stack run"

    chatbot_directory = pathlib.Path(__file__).parent.absolute()
    print(chatbot_directory)
    parent_directory = chatbot_directory.parent
    print(parent_directory)
    lci_directory_string = str(parent_directory) + "/LCISearch"
    print(lci_directory_string)

    json_argument = LCI_FILE_NAME
    search_arguments = " ".join(attribute_list)
    command = LCI_SEARCH_STRING + " " + json_argument + " " + search_arguments
    print("We call: " + command)
    subprocess.call(command, cwd=lci_directory_string, shell=True)

    read_path = lci_directory_string + "/ausgabe.json"
    # lci_search_response = open(read_path, 'r').read()
    with open(read_path, 'r', encoding='utf-8') as ausgabe_json:
        lci_search_response = json.load(ausgabe_json)
    print("LCI search response: " + json.dumps(lci_search_response))
    return lci_search_response

def search_for_saint(message):
    sentence_words = nltk.word_tokenize(message)
    print(sentence_words)
    lemmatized_words = [hannover.analyze(word) for word in sentence_words]
    print(lemmatized_words)
    #for lw in lemmatized_words:
    #    print(lw[0])
    #    print(lw[1])
    nouns_in_message = [lw[0] for lw in lemmatized_words if 'NN' == lw[1] or "NE" == lw[1]]
    print(nouns_in_message)

    global feedback_mode
    global last_input_nouns
    if feedback_mode:
        feedback_mode = False
        nouns_in_message = nouns_in_message + last_input_nouns
        last_input_nouns = []

    response = None
    if len(nouns_in_message) > 0:
        noun_string = ", ".join(nouns_in_message)
        print(noun_string)
        lci_search_response = call_LCISearch(nouns_in_message)

        print("DEBUGGING: RESPONSE LCI SEARCH: ", lci_search_response)

        response = "Ich habe mit den Attributen \" " + noun_string + " \" nach einem Heiligen gesucht."
        if len(lci_search_response) > 0:
            sorted_list = sorted(lci_search_response, key=lambda k: k['w_Count'], reverse=True)
            print("DEBUGGING: sorted list: ", sorted_list)

            max_count = sorted_list[0].get('w_Count')
            amount_of_max_count = 0
            for element in sorted_list:
                if element.get('w_Count') == max_count:
                    amount_of_max_count += 1

            max_saints_in_response = 5

            if amount_of_max_count > max_saints_in_response:
                last_input_nouns = nouns_in_message
                feedback_mode = True

                response = response + "\nZuviele Ergebnisse gefunden: Gib bitte zusätzliche Eigenschaften an"

            else:
                top_three_dicts = sorted_list[:amount_of_max_count]
                top_three_saints = [element['name'] for element in top_three_dicts]
                print(top_three_dicts)
                print("DEBUGGING: Top drei Heilige: ", top_three_saints)
                response = response + "\nErgebnis:"
                for saint in top_three_saints:
                    response = response + "\n" + saint

            print("DEBUGGING: Response for User: ", response)

        else:
            response = response + "\n\nEs konnten keine Heiligen für die Eingabe gefunden werden!\n Versuche es erneut mit mehr Begriffen!"

    else:
        #print("Entschuldigung! Das habe ich nicht verstanden.")
        response = "Entschuldigung! Das habe ich nicht verstanden."
    print(response)
    return response

#while True:
#    message = input("")
#    ints = predict_class(message)
#    res = None
#
#    if len(ints) == 0:
#        search_for_saint(message)
#    else:
#       res = get_response(ints, intents)
#        print(res)

def chatbot_response(input_text):
    ints = predict_class(input_text)
    res = None

    if len(ints) == 0:
        res = search_for_saint(input_text)
    else:
        res = get_response(ints, intents)

    return res

app = Flask(__name__, template_folder='templates')
app.static_folder = 'static'

@app.route("/")
def home():
    return render_template("index.html")

@app.route("/get")
def get_bot_response():
    inputText = request.args.get('msg')
    return chatbot_response(inputText)

if __name__ == "__main__":
    app.run()