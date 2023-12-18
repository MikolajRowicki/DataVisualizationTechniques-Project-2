% Poniższy skrypt prezentuje działanie funkcji metodaHalleya oraz
% wizualizacjaZbieznosci, a także wielu innych pobocznych funkcji na 
% 6 nietrywialnych przykładach wielomianów zapisanych w bazie wielomianów 
% Czebyszewa pierwszego rodzaju. Funkcje generujące wizualizacje zostały
% zakomentowane, aby usprawnić działanie programu
% xi oznacza wektor x w i-tym przykladzie
% xia oznacza wektor x w podpunkcie a przykladu i

% Przykład 1

% Będziemy rozważać wielomiany zapisane w bazie wielomianów Czebyszewa
% pierwszego rodzaju, a wektor a, którym będziemy się tu posługiwać, 
% będzie wektorem współczynników danego wielomianu w tej bazie. Zacznijmy 
% od czegoś naprawdę prostego na rozgrzewkę. Niech

a1 = [0,0,1];
%wizualizacjaZbieznosci(a, -20, 20, -20, 20, 500, 500);

% Widzimy, że metoda nie jest zbieżna, gdy część rzeczywista przybliżenia
% początkowego jest równa 0. Sprawdźmy zatem, co dzieje się dla x = 0.
% Niech : 
x1 = [0, 0 + 1i, 0 - 1i];
% Wówczas wartości pierwszej pochodnej ...
dp = wartosciPierwszejPochodnej(a1,x1);
[y1,k1] = metodaHalleya(x1,a1);
[e11,e21] = sprawdzBlad(a1,x1,y1,k1);
e11;
e21;

% też są równe 0. Wówczas we wzorze metody Halleya mamy dzielenie przez 0,
% metoda nie jest zbieżna

% Przykład 2

% Niech współczynniki naszego wielomianu w bazie Czebyszewa będą kolejnymi
% wyrazami ciągu Fibonacciego.

a2 = [0,1,1,2,3,5,8,13,21,34,55];

%Z wykresu liczby iteracji możemy wyczytać, że im dalej punkty oddalone są
% od punktu (0,0), tym więcej iteracji jest potrzebnych, by użyskać żądane
% przybliżenie miejsca zerowego. Sprawdźmy, czy ta intuicja sprawdza się w
% rzeczywistości. Ustalmy, że Im(x) = 0 i idźmy w prawo wzdłuż osi
% rzeczywistej, sprawdzając, jak będzie zachowywała się metoda Halleya dla
% coraz większych co do modułu x. Sytuację tę ilustruje poniższa tabela.

% wizualizacjaZbieznosci(a, -20, 20, -20, 20, 500, 500);

x2 = [0,1,2,5,10,50,100,1000,10000,10000000];
[y2,k2] = metodaHalleya(x2,a2);
[e12,e22] = sprawdzBlad(a2,x2,y2,k2);
e12;
e22;

% Przykład 3

% W poprzednim przykładzie zaciekawienie budzi symetria powstałej na 
% wykresie struktury. Zobaczmy, czy na innym przykładzie również zobaczymy 
% podobną właśność. Tym razem rozważmy zwykłe wielomiany Czebyszewa 
% pierwszego rodzaju, to znaczy niech wektor $a$ będzie wektorem 
% zawierającym same jedynki.
% Niech:

a3 = [1,1,1,1,1,1,1,1,1,1,1,1,1];

% wizualizacjaZbieznosci(a, -20, 20, -20, 20, 500, 500);

%Ponownie otrzymujemy płaszczyznę zespoloną podzieloną na kilka części 
% prostymi. Jeżeli w metodzie Halleya zdecydujemy się wybrać przybliżenie
% początkowe leżące wzdłuż tej prostej, będziemy musieli wykonać stosunkowo
% dużo operacji, więcej niż dla innych elementów o podobnym module. Nie
% tylko więc duży moduł przybliżenia początkowego negatywnie wpływa na
% zbieżność metody Halleya. Rozważmy punkty leżące na okręgu na płaszczyźnie.


theta1 = linspace(0, 2*pi, 10);
x3a = exp(1i * theta1) * 30;
[y3a,k3a] = metodaHalleya(x3a,a3);
[e13a,e23a] = sprawdzBlad(a3,x3a,y3a,k3a);
e13a;
e23a;


theta2 = linspace(0, 2*pi, 10);
x3b = exp(1i * theta2) * 5;
[y3b,k3b] = metodaHalleya(x3b,a3);
[e13b,e23b] = sprawdzBlad(a3,x3b,y3b,k3b);
e13b;
e23b;

% Widzimy, że dla większego r są znacznie większe rozbieżności między 
% liczbą iteracji dla poszczególnych przybliżeń początkowych. 

% Przykład 4

a4 = [1,2,3,4,5,6,7,8,9,10];

%wizualizacjaZbieznosci(a, -1, 1, -1, 1,1000, 1000);

% Tym razem otrzymujemy nieco inną strukturę. Liczba iteracji zdaje się 
% rosnąć szczególnie wzdłuż osi zespolonej.

%Widzimy, że gdy część zespolona rozważanej liczby jest równa 0, liczba
% potrzebnych iteracji jest mniejsza niż w przypadku, gdy część zespolona
% jest niezerowa, zaś zerowa jest część rzeczywista. Przedstawione 
% własności są jednak wyłącznie lokalnie, nie da się ich przełożyć na całą 
% płaszczyznę zespoloną. Wzdłuż widocznych na rysunku linii (tym razem 
% nawet nie przypominają one prostych), iteracja przebiega zupełnie inaczej
% niż w przypadku innych liczb. Nie to tylko punkty te wymagają więcej 
% iteracji, ale również zbiegają do zupełnie innych pierwiastków niż ich
% sąsiedzi. Zauważalne są też charakterystyczne struktury, zwane fraktalami.

%wizualizacjaZbieznosci(a, 0.053, 0.063, 0.1, 0.11, 1000, 1000);

% Otrzymaliśmy jeden z fraktali Halleya. Na naszym obrazku pojawiają się
% zupełnie nowe fraktale, o których wcześniej nie mieliśmy pojęcia. 
% Przybliżając tak nasz wykres w nieskończoność, otrzymamy analogiczny, 
% nieskończony ciąg fraktali.

x4 = [-1,1,0,-1i,1i];
[y4,k4] = metodaHalleya(x4,a4);
[e14,e24] = sprawdzBlad(a4,x4,y4,k4);
e14;
e24;

% Przykład 5

% Metoda Halleya jest naturalnym generatorem fraktali Halleya. Fraktale od
% lat budzą zainteresowanie matematyków, niektórym jednak poświęcana jest
% szczególna uwaga. Jednym z przykładów takich jest fraktal powstały przez
% zastsosowanie metody Halleya do znalezienia miejsc zerowych wielomianu 
% postaci w(z)=z^p-1. Rozważmy więc funkcję f(x) = z^3 -1. Czy da się
% ją zapisać w bazie wielomianów Czebyszewa pierwszego rodzaju? Oczywiście!
% Wektor współczynników a będzie miał następującą postać

a5 = [-1,3/4,0,1/4];
% wizualizacjaZbieznosci(a, -20, 20, -20, 20, 500, 500);

% Otrzymany na rysunku fraktal jest jednym z najbardziej znanych i nazywany
% jest fraktalem normalnym Halleya.

% Przykład 6

% Na koniec przyjrzyjmy się jeszcze jednemu bardzo ciekawemu fraktalowi.

a6 = [-10,24,35,46,21];

%wizualizacjaZbieznosci(a, -1, 0.5, -0.3, 0.3, 500, 500);

% W powyższym fraktalu możemy zauważyć, że wzdłuż osi rzeczywistej 
% przechodzi linia o zupełnie innym kolorze niż jej otoczenei. Mianowicie 
% dla liczb rzeczywistych liczba koniecznych do wykonania jest dużo większa
% niż dla bliskim im liczbom zespolonym. Niech zilustruje to poniższa 
% tabela:

x6 = [-0.8,-0.3,0, -0.8 + 0.1i, -0.3 + 0.1i, 0.1i];
[y6,k6] = metodaHalleya(x6,a6);
[e16,e26] = sprawdzBlad(a6,x6,y6,k6);
e16;
e26;

% Okazuje się, że dla x rzeczywistych potrzebujemy bardzo dużo iteracji, 
% aby uzyskać wymagane przybliżenie miejsca zerowego. Wystarczy jednak, 
% że dodamy do nich różną od 0 część zespoloną i liczba koniecznych do 
% wykonania iteracji będzie już znacznie mniejsza.

