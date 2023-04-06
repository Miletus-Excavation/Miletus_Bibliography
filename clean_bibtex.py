import bibtexparser
from bibtexparser.bparser import BibTexParser
from bibtexparser.bwriter import BibTexWriter
from bibtexparser.customization import homogenize_latex_encoding, convert_to_unicode

with open('data/Milet_Bibliography_BibTeX.bib', encoding="utf8") as bibtex_file:
    parser = BibTexParser()
    parser.encoding = "utf-8"
    parser.ignore_nonstandard_types = False
    parser.homogenize_fields = False
    parser.common_strings = False
    parser.customization = homogenize_latex_encoding
    bib_database = bibtexparser.load(bibtex_file, parser=parser)


# print(bib_database.entries)

writer = BibTexWriter()
writer.order_entries_by=None
writer.display_order
with open('data/Milet_Bibliography_BibTeX.bib', 'w', encoding='utf-8') as bibfile:
    bibfile.write(writer.write(bib_database))
