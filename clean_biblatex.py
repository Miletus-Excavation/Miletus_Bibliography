import bibtexparser
from bibtexparser.bparser import BibTexParser
from bibtexparser.bwriter import BibTexWriter

with open('data/Milet_Bibliography_BibLaTeX.bib', encoding="utf8") as bibtex_file:
    parser = BibTexParser()
    parser.encoding = "utf-8"
    parser.ignore_nonstandard_types = False
    parser.homogenize_fields = True
    parser.common_strings = False
    bib_database = bibtexparser.load(bibtex_file, parser=parser)


# print(bib_database.entries)

writer = BibTexWriter()
writer.order_entries_by = None #('author', 'year')
with open('data/Milet_Bibliography_BibLaTeX.bib', 'w', encoding='utf-8') as bibfile:
    bibfile.write(writer.write(bib_database))
