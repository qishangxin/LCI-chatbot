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
                         '�KT']

def remove_numbers(text):
    # define the pattern to keep
    pattern = r'[^a-zA-Z�������.,!?/:;\"\'\s]'
    return re.sub(pattern, ' ', text)


def remove_special_characters(text):
    # define the pattern to keep
    pat = r'[^a-zA-Z0-9�������.!?:\s]'
    return re.sub(pat, '', text)


def remove_first_and_end_spaces(string):
    # Remove the first and end spaces
    return "".join(string.rstrip().lstrip())  # evtl .rstrip() weglassen


def remove_cid_from_end_of_line(text):
    # Entfernt den Ausdruck "cid:", der f�r einen Bindestrich steht
    return re.sub(r'cid:', '', text)


def remove_extra_whitespaces(text):
    # entfernt �berfl�ssige whitespaces
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
        text = re.sub(r"^([A-Z���]{1})\s([a-z����]{2,})", r"\1\2", text)  # f�gt W�rter am Zeilenanfang zusammen (K �ln -> K�ln)
        text = re.sub(r"(\s[A-Z���]{1})\s([a-z����]{2,})", r"\1\2", text)  # f�gt W�rter zusammen (_K_�ln -> _K�ln)
        text = re.sub(r"^\. ", "", text)  # l�scht Punkte am Zeilenanfang
        text = re.sub(r"^[a-z]\.\s", "", text)  # entfernt Abk�rzungen am Zeilenanfang
        text = re.sub(r"^[CIVXML]{2,}\.\s", "", text)  # l�scht r�m. Zahlen am Zeilenanfang,
        text = re.sub(r"^[CIVXML]{2,}\.\s?", "", text)  # damit unser Algo. f�r die Heiligennamen-Extraktion
        text = re.sub(r"^[CIVXML]{2,}\s\.\s?", "", text)  # funktioniert (siehe: Gro�buchstaben am Zeilenanfang)
        text = re.sub(r"\s\. ", " ", text)  # ersetzt unbedeutsame Punkte mit Leerzeichen
        text = re.sub(r"\s\.$", "", text)  # l�scht alleinstehende Punkte am Zeilenende
        text = re.sub(r"^II\s", "", text)  # entfernt r�m. Zahlen
        text = re.sub(r"^[IVXMLC]{2,} ", "", text)  # entfernt r�m. Zahlen
        text = re.sub(r"^[IVXMLC]{2,}\s", "", text)  # entfernt r�m. Zahlen

        for abbrevation in critical_abbrevations:
            text = re.sub(r"^" + abbrevation + "\s", "", text)
        x += 1

    return text


with open(filename, 'r') as fn:
    for line in fn:
        if line.rstrip():
            line = pre_process(line)
            if len(line) > 2:  # �berspringt leerzeilen und andere unn�tige Zeilen
                print(line, file=output)

