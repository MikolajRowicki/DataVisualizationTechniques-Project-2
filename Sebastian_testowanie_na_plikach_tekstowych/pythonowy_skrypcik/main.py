import pathlib, re
from collections import Counter

def main():

    # # Tworzy listę ścieżek do wszystkich plików od podanej ścieżki, mających dane rozszerzenie
    sciezkiDoPlikow = list(pathlib.Path('C:/Users/Sebastian/Desktop/Sebastian_testowanie_na_plikach_tekstowych/plikiDoTestowania').rglob('*.txt'))

    # Zwraca stringa z zamieninymi określonymi elementami
    def replace_all(repls, str):
        return re.sub('|'.join(re.escape(key) for key in repls.keys()),
                      lambda k: repls[k.group(0)], str)

    listaLinii = []
    licznikWyrazow = Counter()
    licznikLiter = Counter()
    listaWyrazowMiedzyDrzewami = []
    with (open('C:/Users/Sebastian/Desktop/Sebastian_testowanie_na_plikach_tekstowych/out.txt', 'w') as out):
        for sciezkaDoPliku in sciezkiDoPlikow:
            with open(sciezkaDoPliku, 'r', encoding='utf-8') as plik:
                tekstZPliku = plik.read()
                out.write(f"[ŚCIEŻKA DO PLIKU] {sciezkaDoPliku}\n[ZAWARTOŚĆ  PLIKU]\n{tekstZPliku}\n\n")
                for linia in tekstZPliku.splitlines():
                    listaLinii.append(linia)
                for wyraz in tekstZPliku.split():
                    licznikWyrazow[wyraz] += 1
                for litera in tekstZPliku:
                    licznikLiter[litera] += 1

                # Zliczanie wyrazów między drzewami (wyrazy między wyrazami: "drzewo1" i "drzewo2")
                wyrazyWPliku = []
                for wyraz in tekstZPliku.split():
                    wyrazyWPliku.append(wyraz)
                if len(wyrazyWPliku) >= 3:
                    elementPoprzedzajacy = wyrazyWPliku[0]
                    for i in range(1, len(wyrazyWPliku)-1):
                        # pierwszy sposób usuwania znaków z wyrazów
                        if elementPoprzedzajacy.replace(".", "").replace(",", "").replace(":", "").replace(";", "").replace("?", "").replace("!", "").replace("...", "").replace("\"", "").replace("\'", "").replace("`!`", "") == "drzewo1":
                            listaRobocza2 = []
                            for j in range(i, len(wyrazyWPliku)-1):
                                # drugi sposób usuwania znaków z wyrazów
                                if replace_all({".": "", ",": "", ":": "", ";": "", "?": "", "!": "", "...": "", "\"": "", "\'": "", "`": ""}, wyrazyWPliku[j]) != "drzewo2":
                                    listaWyrazowMiedzyDrzewami.append(wyrazyWPliku[j])
                                else:
                                    break
                        elementPoprzedzajacy = wyrazyWPliku[i]

        # licznikLiter.most_common() # metoda sortuje obiekt Counter ze względu na ilość wystąpieć, ten porządek jest domyślny
        # licznikLiter = sorted(licznikLiter.items()) # zmienia obiekt Counter na listę, elementy są posortowane alfebetycznie, przydatne do porównywania
        out.write(f"Linijki: {listaLinii}\n")
        out.write(f"Zliczone wyrazy: {licznikWyrazow}\n")
        out.write(f"Zliczone litery: {licznikLiter}\n")
        out.write(f"Wyrazy stojące między drzewami : {listaWyrazowMiedzyDrzewami}\n")


if __name__ == '__main__':
    main()
