import re

FILE_NAME = "abkuerzungen_full.txt"
SAINT_PATTERN = re.compile("^[A-ZÜÄÖß][A-ZÜÄÖß]+$")

file = open(FILE_NAME, 'r', encoding='utf-8')
output = open('critical_abbrevations.txt', 'w', encoding='utf-8')
lines = file.readlines()

dangerous_cases = []
for line in lines:
    words = line.split(" ")
    first_word = words[0]
    matches_saint_pattern = SAINT_PATTERN.search(first_word)
    if matches_saint_pattern:
        dangerous_cases.append(first_word)

dangerous_cases = set(dangerous_cases)
dangerous_cases = list(dangerous_cases)
dangerous_cases.sort()

for case in dangerous_cases:
    print(case, file=output)
