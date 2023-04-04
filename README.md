# LCI-chatbot
LCI(Lexicon of Christian Iconography)-chatbot project: a chatbot that suggests the user of the possible Christian icons based on the user's description.

# Table of Contents
* [Setup](#setup)
* [Use Instructions](#use-instructions)
* [Description](#description)
* [Data](*data)
* [Modules](#modules)
* [Others](#others)

## Setup

### Setup Virtual Environment
```
!git init
!git clone https://github.com/qishangxin/LCI-chatbot.git
```
### Requirements
Install requirements.txt <br />
```
pip install -r requirements.txt
```
Install setup.py <br />
```
python setup.py install
```

## Use Instructions
Run [main.hs](#https://github.com/qishangxin/LCI-chatbot/blob/main/LCISearch/app/Main.hs) and input the features of the Christian icon.
(Note: the chatbot only recognizes German.)


## Description
This project aims at developing a chatbot that helps users to recognize the christian icons they see based on natural language process methods. After receiving the user's description of the icon, our chatbot extract the features from the input and finds the saints with the highest number corresponding features from the large amount of LCI(Lexicon of Christian Iconography) data, which contains more than 4000 chiristian icons. In the end we achieved high accuracy on recognizing the text data using NLP methods and the chatbot can suggest the correct christian icon for over 50% possibility.


## Data
[row data](#https://github.com/qishangxin/LCI-chatbot/blob/main/preprocessing/raw_data.txt) is the original text data extracted from a pdf file containing the information of over 4000 christian icons.


## Modules
The project owns the following modules:

* [Preprocessing](#Preprocessing) 
* [Chatbot development](#Chatbot-development) 
* [LCI search](#LCI-search) 


### Preprocessing
Preprocessing finds the names of the saints and the description of the saints following their names from the text data.


### Chatbot development
This part is the development and training process of the chatbot which allows it to work in the German language and gives it different functions, for example differentiating if the user wants to have a daily conversation or to search for a christian icon and answering correspondingly.

### LCI search
The LCI search function picks out the key words from the user's input and finds the saints with such features.


[(Back to top)](#table-of-contents)
