import pathlib, re, csv
from collections import Counter
import pandas as pd

def stworz_liste_linii(tekst_z_pliku, lista_linii):
    for linia in tekst_z_pliku.splitlines():
        lista_linii.append(linia)
    return lista_linii

def stworz_licznik_wyrazow(tekst_z_pliku, licznikWyrazow):
    for wyraz in tekst_z_pliku.split():
        licznikWyrazow[wyraz] += 1
    return licznikWyrazow

def stworz_licznik_znakow(tekst_z_pliku, licznik_znakow):
    for znak in tekst_z_pliku:
        licznik_znakow[znak] += 1
    return licznik_znakow

def stworz_licznik_mniejszeLubRowne_wiekszeLubRowne_mniejsze_wieksze(licznik_wyrazow):
    licznik = Counter()
    for wyraz in licznik_wyrazow:
        # operator między liczbami [0-9 ][><]=?[0-9 ]
        # operator między liczbą, a wyrazem [0-9 ][><]=?
        # operator między wyraze,, a liczbą [><][0-9 ]=?
        # operator '<' i '>' może być częścią nawiasu
        if re.search("<=", wyraz):
            licznik["<="] += licznik_wyrazow[wyraz]
        elif re.search(">=", wyraz):
            licznik[">="] += licznik_wyrazow[wyraz]
        elif re.search("<[0-9 ]", wyraz):
            licznik["<"] += licznik_wyrazow[wyraz]
        elif re.search("[0-9 ]>", wyraz):
            licznik[">"] += licznik_wyrazow[wyraz]
        elif re.search("==", wyraz):
            licznik["=="] += licznik_wyrazow[wyraz]
    return licznik

def stworz_licznik_wyrazow_java(licznik_wyrazow):
    licznik = Counter()
    licznik["private"] = licznik_wyrazow["private"]
    licznik["public"] = licznik_wyrazow["public"]
    licznik["protected"] = licznik_wyrazow["protected"]
    licznik["abstract"] = licznik_wyrazow["abstract"]
    licznik["final"] += licznik_wyrazow["final"]
    licznik["static"] += licznik_wyrazow["static"]
    licznik["default"] += licznik_wyrazow["default"]
    licznik["void"] += licznik_wyrazow["void"]

    licznik["byte"] = licznik_wyrazow["abstract"]
    licznik["short"] += licznik_wyrazow["short"]
    licznik["int"] += licznik_wyrazow["int"]
    licznik["long"] += licznik_wyrazow["long"]
    licznik["float"] = licznik_wyrazow["float"]
    licznik["double"] += licznik_wyrazow["double"]
    licznik["boolean"] += licznik_wyrazow["boolean"]
    licznik["char"] += licznik_wyrazow["char"]
    licznik["String"] += licznik_wyrazow["String"]

    licznik["class"] = licznik_wyrazow["class"]
    licznik["enum"] += licznik_wyrazow["enum"]
    licznik["interface"] += licznik_wyrazow["interface"]
    licznik["annotation"] += licznik_wyrazow["annotation"]

    licznik["extends"] = licznik_wyrazow["extends"]
    licznik["new"] = licznik_wyrazow["new"]

    for wyraz in licznik_wyrazow:
        if re.search("if", wyraz):
            licznik["if"] += licznik_wyrazow[wyraz]
    licznik["else"] += licznik_wyrazow["else"]
    licznik["return"] += licznik_wyrazow["return"]
    licznik["throw"] += licznik_wyrazow["throw"]
    licznik["throws"] = licznik_wyrazow["throws"]
    licznik["import"] += licznik_wyrazow["import"]
    licznik["catch"] += licznik_wyrazow["catch"]
    licznik["package"] += licznik_wyrazow["package"]

    return licznik

def stworz_licznik_ilosci_linii_odstepu(tekst_z_pliku, licznik):
    ilosc_odstepow_po_kolei = 0
    for wyraz in tekst_z_pliku:
        if wyraz == "\n":
            ilosc_odstepow_po_kolei += 1
            licznik["Liczba linijek odstepu"] += 1
        else:
            if ilosc_odstepow_po_kolei == 0:
                licznik["Brak odstepu"] += 1
            elif ilosc_odstepow_po_kolei == 1:
                licznik[f"{ilosc_odstepow_po_kolei} linijka odstepu"] += 1
                ilosc_odstepow_po_kolei = 0
            else:
                licznik[f"{ilosc_odstepow_po_kolei} linijki odstepu"] += 1
                ilosc_odstepow_po_kolei = 0
    if ilosc_odstepow_po_kolei != 0:
        licznik[f"{ilosc_odstepow_po_kolei} linijki odstepu"] += 1
    return licznik

def replace_all(repls, str):
    return re.sub('|'.join(re.escape(key) for key in repls.keys()), lambda k: repls[k.group(0)], str)

def main():

    # # Ścieżki, z których korzysta program
    # # To należy zmienić
    sciezka_do_folderu_z_plikami_java = 'C:/Users/Sebastian/Desktop/Semestr 3/Zaawansowane programowanie obiektowe i funkcyjne/Laboratorium'
    sciezka_do_miejsca_zapisu_plikow = 'C:/Users/Sebastian/Desktop/Semestr 3/Techniki wizualizacji danych/Projekt/PRO_2/skrypt_java'
    imie = 'Sebastian'

    # # Tworzy listę ścieżek do wszystkich plików od podanej ścieżki, mających dane rozszerzenie
    sciezki_do_plikow = list(pathlib.Path(sciezka_do_folderu_z_plikami_java).rglob('*.java'))

    # # Listy i liczniki, w których będą przechowywane aliczane statystyki
    lista_linii = []
    licznik_wyrazow = Counter()
    licznik_znakow = Counter()
    licznik_ilosci_linii_odstepu = Counter()

    # # Tworzy plik tekstowy zawierający wybrane statystyki
    with open(f'{sciezka_do_miejsca_zapisu_plikow}/out.txt', 'w') as out:
        for sciezka_do_pliku in sciezki_do_plikow:
            with open(sciezka_do_pliku, 'r', encoding='utf-8') as plik:
                tekst_z_pliku = plik.read()
                # out.write(f"[ŚCIEŻKA DO PLIKU] {sciezka_do_pliku}\n[ZAWARTOŚĆ  PLIKU]\n{tekst_z_pliku}\n\n")

                # # Zapełnia utworzone listy i liczniki
                lista_linii = stworz_liste_linii(tekst_z_pliku, lista_linii)
                licznik_wyrazow = stworz_licznik_wyrazow(tekst_z_pliku, licznik_wyrazow)
                licznik_znakow = stworz_licznik_znakow(tekst_z_pliku, licznik_znakow)
                licznik_ilosci_linii_odstepu = stworz_licznik_ilosci_linii_odstepu(tekst_z_pliku, licznik_ilosci_linii_odstepu)
        licznik_mniejszeLubRowne_wiekszeLubRowne_mniejsze_wieksze = stworz_licznik_mniejszeLubRowne_wiekszeLubRowne_mniejsze_wieksze(licznik_wyrazow)
        licznik_wyrazow_java = stworz_licznik_wyrazow_java(licznik_wyrazow)

        # # Wypisuje do pliku tekstowego zawartości list i liczników
        # out.write(f"Linijki: {lista_linii}\n\n")
        # out.write(f"Zliczone wyrazy: {licznik_wyrazow}\n\n")
        # out.write(f"Zliczone litery: {licznik_znakow}\n\n")
        out.write(f"Operatory porządku: {licznik_mniejszeLubRowne_wiekszeLubRowne_mniejsze_wieksze}\n\n")
        out.write(f"Wybrane wyrazy: {licznik_wyrazow_java}\n\n")
        out.write(f"Liczba linii odstępu: {licznik_ilosci_linii_odstepu}\n\n")


        # # Zapisywanie danych do pliku java_[IMIĘ].csv
        with open(f'{sciezka_do_miejsca_zapisu_plikow}/java_{imie}.csv', 'w') as csv_out:
            csv_out.write(f'Element;Count_{imie}\n')
            for element in licznik_mniejszeLubRowne_wiekszeLubRowne_mniejsze_wieksze:
                csv_out.write(f'{element};{licznik_mniejszeLubRowne_wiekszeLubRowne_mniejsze_wieksze[element]}\n')
            for element in licznik_wyrazow_java:
                csv_out.write(f'{element};{licznik_wyrazow_java[element]}\n')
            for element in licznik_ilosci_linii_odstepu:
                csv_out.write(f'{element};{licznik_ilosci_linii_odstepu[element]}\n')

        # # Łączenie wszystkich plików java_[IMIĘ].csv w jeden plik java.csv
        # # imie2 i imie3 do zmiany przy łączeniu ramek danych w jedną
        # imie2 = imie
        # imie3 = imie
        # with open(f'{sciezka_do_miejsca_zapisu_plikow}/java.csv', 'w') as csv_out:
        #     df1 = pd.read_csv(f'{sciezka_do_miejsca_zapisu_plikow}/java_{imie}.csv', sep=';')
        #     df2 = pd.read_csv(f'{sciezka_do_miejsca_zapisu_plikow}/java_{imie2}.csv', sep=';')
        #     df3 = pd.read_csv(f'{sciezka_do_miejsca_zapisu_plikow}/java_{imie3}.csv', sep=';')
        #     pd.merge(pd.merge(df1, df2, on='Element', how='outer'), df3, on='Element', how='outer').\
        #         to_csv(f'{sciezka_do_miejsca_zapisu_plikow}/java.csv', sep=';', index=False)






if __name__ == '__main__':
    main()
