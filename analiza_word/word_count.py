from datetime import datetime
from pathlib import Path

import docx2txt
import re
import os

import pandas as pd


# zliczanie wszystkich słów
def count_words(docx_path):
    result = docx2txt.process(docx_path)
    lista = result.split()
    return len(lista)


# zliczanie wszystkich kropek
def count_dots(docx_path):
    result = docx2txt.process(docx_path)
    return result.count(".")


# zliczanie wszystkich przecinkow
def count_przecinki(docx_path):
    result = docx2txt.process(docx_path)
    return result.count(",")

# zliczanie wszystkich wykrzyknikow
def count_wykrzyknik(docx_path):
    result = docx2txt.process(docx_path)
    return result.count("!")

# zliczanie wszystkich znkaow zapytania
def count_pytajnik(docx_path):
    result = docx2txt.process(docx_path)
    return result.count("?")

# zliczanie wszystkich myslnikow
def count_myslnik(docx_path):
    result = docx2txt.process(docx_path)
    return result.count("-")

# zliczanie wszystkich dwukropkow
def count_dwukropek(docx_path):
    result = docx2txt.process(docx_path)
    return result.count(":")


# zliczanie innych znaków interpunkcyjnych
def count_others(docx_path):
    result = docx2txt.process(docx_path)
    return result.count(";") + result.count("'") + result.count("...")


# zlozonosc zdan
def zlozonosc_zdan(docx_path):
    result = docx2txt.process(docx_path)
    wielokropek = result.count("...")
    liczba_zdan = result.count(".") - 2 * wielokropek
    if liczba_zdan == 0:
        return None
    return count_przecinki(docx_path) / liczba_zdan


# znajdowanie najdłuższego zdania w tekscie -> mało sensu, ponieważ nie wszystkie pliki są w zdaniach
def znajdz_najdluzsze_zdanie(docx_path):
    result = docx2txt.process(docx_path)
    lista_zdan = re.split(r'[.!?]', result)
    najwiecej_slow_zdanie = max(lista_zdan, key=lambda zdanie: len(re.findall(r'\b\w+\b', zdanie)))
    return najwiecej_slow_zdanie


# znajdowanie najdluzszego słowa w tekście
def znajdz_najdluzsze_slowo(docx_path):
    result = docx2txt.process(docx_path)
    lista = result.split()
    lista = [s.replace(',', '').replace('\n', '').replace(".", "").replace(":", "") for s in lista]
    return max(lista, key=len)


def main():
    #od razu tworzę ramkę danych z wszystkimi potrzebnymi kolumnami i zapisuję do pliku csv

    folder_path = './docx/'
    data = []

    if os.path.exists(folder_path) and os.path.isdir(folder_path):
        files = os.listdir(folder_path)
        for el in files:
            #file_stat = Path(folder_path + el).stat()
            modify_time = os.path.getmtime(folder_path + el)
            modify_time = datetime.fromtimestamp(modify_time)
            #creation_time = file_stat.st_ctime
            #creation_date = datetime.fromtimestamp(creation_time).strftime('%Y-%m-%d')
            data.append({"Imię": "Malgosia",
                         "Data utworzenia": modify_time,
                         "Rozszerzenie": "docx",
                         "Nazwa pliku": el,
                         "Ilość słów:": count_words(folder_path + el),
                         "Ilość przecinków": count_przecinki(folder_path + el),
                         "Ilość pytajnikow": count_pytajnik(folder_path + el),
                         "Ilość wykrzyknikow": count_wykrzyknik(folder_path + el),
                         "Ilość kropek": count_dots(folder_path + el),
                         "Ilość myslnikow": count_myslnik(folder_path + el),
                         "Ilość dwukropkow": count_dwukropek(folder_path + el),
                         "Ilość pozostałych znaków": count_others(folder_path + el),
                         "Złożoność zdań": zlozonosc_zdan(folder_path + el)})
    df = pd.DataFrame(data)
    # df.to_csv("aaa")


if __name__ == "__main__":
    main()

