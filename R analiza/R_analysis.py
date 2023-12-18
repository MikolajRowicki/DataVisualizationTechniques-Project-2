import os
import pathlib
import re
from datetime import datetime

import pandas as pd


# tworzy listę uzywanych pakietów w danym pliku
def packages(path):
    library = []
    try:
        with open(path, 'r', encoding='utf-8') as file:
            for line in file:
                line = line.strip()
                if line.startswith("library("):
                    l = line.replace("library(", "").replace(")", "")
                    library.append(l)
    except PermissionError as e:
        return None
    return library


# oblicza stosunek zakomentowanych linii do wszystkich linii
def comment_lines(path):
    comment = 0
    try:
        all_lines = count_lines(path)
        with open(path, 'r', encoding='utf-8') as file:
            for line in file:
                line = line.strip()
                if line.startswith("#"):
                    comment += 1
    except PermissionError as e:
        return None
    if all_lines == 0:
        return None
    return comment / all_lines


# zlicza wszystkie linijki pliku
def count_lines(path):
    try:
        with open(path, 'r', encoding='utf-8') as plik:
            liczba_linii = sum(1 for linia in plik)
        return liczba_linii
    except PermissionError as e:
        return None

# ilość zadeklarowanych zmiennych
def count_variables(path):
    unikalne_zmienne = set()
    try:
        with open(path, 'r', encoding='utf-8') as plik:
            zawartosc = plik.read()
            wzor = re.compile(r'\b(\w+)\s*<-', re.MULTILINE)
            wyniki = wzor.findall(zawartosc)

            unikalne_zmienne.update(wyniki)
            liczba_zmiennnych = len(unikalne_zmienne)

    except PermissionError as e:
        return None, None

    return unikalne_zmienne, liczba_zmiennnych


# zwraca złożoność średnią zmiennej w pliku (czyli średnia ilość słów użyta do opisu zmiennej)
def variables_complexity(path):
    zmienne, ilosc = count_variables(path)
    if zmienne is None or ilosc is None:
        return None
    zmienne = list(zmienne)
    suma = 0
    for el in zmienne:
        count = el.split("_")
        count = len(count)
        suma += count
    if ilosc == 0:
        return None
    return suma / ilosc


def blank_lines(path):
    blank = 0
    try:
        with open(path, 'r', encoding='utf-8') as file:
            for line in file:
                if line.isspace():
                    blank += 1
    except PermissionError as e:
        return None
    return blank


def data_utworzenia_pliku(path):
    if os.path.exists(path):
        timestamp_utworzenia = os.path.getctime(path)
        data_utworzenia = datetime.fromtimestamp(timestamp_utworzenia)
        return data_utworzenia.strftime('%Y-%m-%d')
    else:
        return None


def main():
    folder_path = "C:\\Users\\HP\\R"
    sciezki_do_plikow = list(pathlib.Path(folder_path).rglob('*.R'))
    imie = "Malgosia"
    data = []

    if os.path.exists(folder_path) and os.path.isdir(folder_path):
        for el in sciezki_do_plikow:
            data.append({"Imie": imie,
                         "Data utworzenia pliku": data_utworzenia_pliku(el),
                         "Rozszerzenie": "R",
                         "Nazwa pliku": el.name,
                         "Ilosc linii": count_lines(el),
                         "Uzyte pakiety": packages(el),
                         "Stosunek zakomentowanych linii": comment_lines(el),
                         "Liczba unikalnych stworzonych zmiennych": count_variables(el)[1],
                         "Srednia zlozonosc zmiennej w pliku": variables_complexity(el),
                         "Ilosc pustych linii": blank_lines(el)
                         })
    df = pd.DataFrame(data)
    # print(df)
    df.to_csv("Malgosia-R.csv")


if __name__ == "__main__":
    main()
