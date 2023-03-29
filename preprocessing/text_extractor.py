#!/usr/bin/python

import os
from tqdm import tqdm
from pdfminer.pdfparser import PDFParser
from pdfminer.pdfdocument import PDFDocument, PDFNoOutlines
from pdfminer.pdfpage import PDFPage
from pdfminer.pdfinterp import PDFResourceManager, PDFPageInterpreter
from pdfminer.converter import PDFPageAggregator
from pdfminer.layout import LAParams, LTTextBox, LTTextLine


def open_pdf(pdf_file, fnct, *args):
    """PDF wird geöffnet und die Funktion wird angewendet."""
    res = None
    try:
        file = open(pdf_file, 'rb')
        parser = PDFParser(file)
        doc = PDFDocument(parser)
        parser.set_document(doc)

        if doc.is_extractable:
            res = fnct(doc, *args)

        file.close()
    except IOError as e:
        print(e.errno)
        print(e)
        pass
    return res


def convert_bytestring (s, enc='utf-8'):
    """ Unicode --> bytestring """
    if s:
        if isinstance(s, str):
            return s
        else:
            return s.encode(enc)


def group_text(g, layout_object, abw=0.2):
    """Gruppiert Textteile anhand der x0, x1 Koordinaten zu Spalten. abw gibt die Abweichtoleranz der Koordinaten in Prozent an"""

    x0 = layout_object.bbox[0]
    x1 = layout_object.bbox[2]

    key_found = False
    for key, value in g.items():
        group_x0 = key[0]
        if x0 >= (group_x0 * (1.0-abw)) and (group_x0 * (1.0+abw)) >= x0:
            group_x1 = key[1]
            if x1 >= (group_x1 * (1.0-abw)) and (group_x1 * (1.0+abw)) >= x1:
                key_found = True
                value.append(convert_bytestring(layout_object.get_text()))
                g[key] = value
    if not key_found:
        g[(x0, x1)] = [convert_bytestring(layout_object.get_text())]

    return g


def parse_layout_objects (layoutObjects, text_content=None):
    """Layout-Objekte durchlaufen und Text extrahieren"""
    if text_content is None:
        text_content = []

    text = {}  # key=(x0, x1) Koordinaten der bbox, value=liste von strings innerhalb der bbox-breite (spalte)
    for layoutObject in layoutObjects:
        if isinstance(layoutObject, LTTextBox) or isinstance(layoutObject, LTTextLine):
            text = group_text(text, layoutObject)

    # sortierung nach (x0,x1) Koordinaten nach dem Muster top-down & left-to-right
    for k, v in sorted([(key,value) for (key,value) in text.items()]):
        text_content.append(''.join(v))

    return '\n'.join(text_content)


def _pageParser(document):

    rsrcmgr = PDFResourceManager()
    laparams = LAParams()
    device = PDFPageAggregator(rsrcmgr, laparams=laparams)
    interpreter = PDFPageInterpreter(rsrcmgr, device)

    text = []
    for i, page in enumerate(PDFPage.create_pages(document)):
        interpreter.process_page(page)
        layout = device.get_result()  # liefert LTPage-Objekt für diese Seite ggbf. mit childobject wie LTTextBox, LTFigure, LTImage, etc.
        text.append(parse_layout_objects(layout))

    return text


def process_pages(pdf_file):
    """Verarbeitet jede Seite der PDF und gibt den Text auf der Seite als eine Liste von Strings zur?ck"""
    return open_pdf(pdf_file, _pageParser)


def main():
    directory = "H:\\HDD\\Uni\\Master\\PDFs"  # Ordner mit allen PDF-Dateien
    outfile = open("H:\\HDD\\Uni\\Master\\raw_data.txt", 'w')

    for filename in tqdm(sorted(os.listdir(directory))):
        if filename.endswith(".pdf"):
            pathname = os.path.join(directory, filename)
            pages = process_pages(pathname)

            for x in range(len(pages)):
                print(pages[x], file=outfile)
        else:
            continue

        print("Done: " + filename)

    outfile.close()

if __name__ == "__main__":
    main()
