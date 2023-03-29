#!/usr/bin/python
# -*- coding: cp1252 -*-

import re


filename = 'raw_data.txt'
output = open('output.txt', 'w')

critical_abbrevations = ['AAS',
                         'AASS',
                         'AB',
                         'ABK',
                         'AEKG',
                         'AFP',
                         'AMB',
                         'AMK',
                         'BBK',
                         'BHG',
                         'BHL',
                         'BHO',
                         'BI',
                         'BN',
                         'BNM',
                         'CIL',
                         'CR',
                         'CRL',
                         'CSEL',
                         'CVUOSB',
                         'DACL',
                         'DHGE',
                         'EMWP',
                         'GBA',
                         'GNM',
                         'HDAG',
                         'HI',
                         'HPB',
                         'III',
                         'ISBN',
                         'IX',
                         'JKS',
                         'KFM',
                         'KGM',
                         'KOM',
                         'LA',
                         'LB',
                         'LM',
                         'LP',
                         'LPI',
                         'LXX',
                         'MF',
                         'MG',
                         'OCD',
                         'OESA',
                         'OFM',
                         'OS',
                         'OSA',
                         'OSB',
                         'OSD',
                         'OSM',
                         'OST',
                         'PG',
                         'PL',
                         'QQQ',
                         'RAC',
                         'RBK',
                         'RDK',
                         'RGG',
                         'RQS',
                         'SS',
                         'TOF',
                         'TOR',
                         'TU',
                         'UND',
                         'ZD',
                         'ZK',
                         'ZSAK',
                         'ZVK',
                         'ÖKT']

def remove_numbers(text):
    # define the pattern to keep
    pattern = r'[^a-zA-ZäÄöÖüÜß.,!?/:;\"\'\s]'
    return re.sub(pattern, ' ', text)


def remove_special_characters(text):
    # define the pattern to keep
    pat = r'[^a-zA-Z0-9äÄöÖüÜß.!?:\s]'
    return re.sub(pat, '', text)


def remove_first_and_end_spaces(string):
    # Remove the first and end spaces
    return "".join(string.rstrip().lstrip())  # evtl .rstrip() weglassen


def remove_cid_from_end_of_line(text):
    # Entfernt den Ausdruck "cid:", der für einen Bindestrich steht
    return re.sub(r'cid:', '', text)


def remove_extra_whitespaces(text):
    # entfernt überflüssige whitespaces
	space_pattern = r'\s+'
	without_space = re.sub(pattern=space_pattern, repl=" ", string=text)
	return without_space



def pre_process(text):

    text = remove_numbers(text)
    text = remove_special_characters(text)
    text = re.sub(r"\s[a-z]\.\s", " ", text)
    text = remove_first_and_end_spaces(text)
    text = remove_cid_from_end_of_line(text)
    text = remove_extra_whitespaces(text)
    for x in range(5):
        text = re.sub(r"^([A-ZÄÖÜ]{1})\s([a-zäöüß]{2,})", r"\1\2", text)  # fügt Wörter am Zeilenanfang zusammen (K öln -> Köln)
        text = re.sub(r"(\s[A-ZÄÖÜ]{1})\s([a-zäöüß]{2,})", r"\1\2", text)  # fügt Wörter zusammen (_K_öln -> _Köln)
        text = re.sub(r"^\. ", "", text)  # löscht Punkte am Zeilenanfang
        text = re.sub(r"^[a-z]\.\s", "", text)  # entfernt Abkürzungen am Zeilenanfang
        text = re.sub(r"^[CIVXML]{2,}\.\s", "", text)  # löscht röm. Zahlen am Zeilenanfang,
        text = re.sub(r"^[CIVXML]{2,}\.\s?", "", text)  # damit unser Algo. für die Heiligennamen-Extraktion
        text = re.sub(r"^[CIVXML]{2,}\s\.\s?", "", text)  # funktioniert (siehe: Großbuchstaben am Zeilenanfang)
        text = re.sub(r"\s\. ", " ", text)  # ersetzt unbedeutsame Punkte mit Leerzeichen
        text = re.sub(r"\s\.$", "", text)  # löscht alleinstehende Punkte am Zeilenende
        text = re.sub(r"^II\s", "", text)  # entfernt röm. Zahlen
        text = re.sub(r"^[IVXMLC]{2,} ", "", text)  # entfernt röm. Zahlen
        text = re.sub(r"^[IVXMLC]{2,}\s", "", text)  # entfernt röm. Zahlen

        for abbrevation in critical_abbrevations:
            text = re.sub(r"^" + abbrevation + "\s", "", text)
        x += 1

    return text


with open(filename, 'r') as fn:
    for line in fn:
        if line.rstrip():
            line = pre_process(line)
            if len(line) > 2:  # überspringt leerzeilen und andere unnötige Zeilen
                print(line, file=output)

