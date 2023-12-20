% Skrypt prezentuje działanie funkcji myhorner(c,x,t) obliczającej 
% uogólnionym algorytmem Hornera wartości wielomianu w punktach t, a także
% funkcji wielowykres(f,a,b,n), która rysuje na jednym obrazku wykres
% funkcji f, jej wielomianu interpolacyjnego, a także wyraźnie zaznacza
% punkty (xk, f(xk)) – gdzie xk to wyznaczone węzły (równoodległe).

% Przykład 1
c = [0,4,5,1,0];
x = [0,1,2,3];
t = [1,2,3];

wynik = myhorner(c,x,t);

% wynik = [4,18,48]

% Przykład 2
x = [0,1,2,3,4,5,6,7,8];
t = [1,2,3,4,5];
c = ilorazr(x, sin(x));

wynik = myhorner(c, x, t);

% wynik = [0.8415, 0.9093, 0.1411, -0.7568, -0.9589]

% Przykład 3
x = [1,2,3,4,5,6,7,8,9,10];
t = [1,2,3,4];
c = ilorazr(x, log(x));

wynik = myhorner(c,x,t);

% wynik = [0, 0.6931, 1.0986, 1.3863]

% Przykład 4

wielowykres(@(x) sin(x), 0, 2 * pi, 10);
 
% Przykład 5

wielowykres(@(x) log(x), 1, 30, 15);

% Przykład 6

wielowykres(@(x) sin(x), 0, 10 * pi, 50);

% Przykład 7

wielowykres(@(x) sin(1./(x + 0.1)) + 0.1*exp(-5*x) + 0.02*x.^3, 0, 20, 10);
