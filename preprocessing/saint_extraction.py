import json
import re

alphabet = ['AÄ', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'OÖ', 'P', 'Q', 'R', 'S', 'T', 'UÜ',
            'V', 'W', 'X', 'Y', 'Z']

first_run_solution = {}
for letter in alphabet:

    current_regex = letter
    current_letter = letter[0]
    saint_pattern = re.compile("^["+current_regex+"][A-ZÜÄÖß]+$")

    file_name = "data_" + current_letter + ".txt"

    file = open(file_name, 'r', encoding='utf-8')
    lines = file.readlines()

    current_saint = None
    current_text_for_saint = []
    for line in lines:
        line = line.strip()
        words = line.split(" ")
        is_author_line = False
        if is_author_line:
            pass
        else:
            first_word = words[0]
            # check if saint line (if first word is the capitalized name of a saint)
            is_saint_line = saint_pattern.search(first_word)
            if is_saint_line:
                if current_saint is None:
                    # case: first saint entry of the file is found
                    current_saint = first_word
                    if len(words) > 1:
                        rest_of_first_line = []
                        for i in range(1, len(words)):
                            rest_of_first_line.append(words[i])
                        rest_of_first_line = " ".join(rest_of_first_line)
                        current_saint = current_saint + " [ " + rest_of_first_line + " ]"
                else:
                    # case: new saint entry found; current entry has to be stored in dict before continuing
                    # put current saint with corresponding collected lines into dict
                    # don't put empty entries in the dict
                    entry_is_empty = len(current_text_for_saint) == 0
                    if entry_is_empty:
                        pass
                    else:
                        first_run_solution[current_saint] = current_text_for_saint
                    # continue with newly found saint
                    current_saint = first_word
                    if len(words) > 1:
                        rest_of_first_line = []
                        for i in range(1, len(words)):
                            rest_of_first_line.append(words[i])
                        rest_of_first_line = " ".join(rest_of_first_line)
                        current_saint = current_saint + " [ " + rest_of_first_line + " ]"
                    current_text_for_saint = []
            else:
                current_text_for_saint.append(line)
    # last saint has to be added here
    first_run_solution[current_saint] = current_text_for_saint

# EASY WAY TO TEST: Uncomment next print and set debug marker on it
# -> then check the first_run_solution dict in the Debugger variables
# print(first_run_solution)
# None

SAINT_JSON_TAG = "saint"
TEXT_JSON_TAG = "text"

with open('saints.json', 'w', encoding='utf-8') as json_file:
    json_input = []
    for key, value in first_run_solution.items():
        current_dict = dict()
        current_dict[SAINT_JSON_TAG] = key
        text_combined = " ".join(value)
        current_dict[TEXT_JSON_TAG] = text_combined
        json_input.append(current_dict)
    json.dump(json_input, json_file, indent=4, ensure_ascii=False)
