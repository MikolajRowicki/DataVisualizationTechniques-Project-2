function  [y,k] = metodaHalleya(x,a)
% METODAHALLEYA Funkcja dla danego wektora przybliżeń początkowych x i dla 
% danego wektora współczynników a wyznacza metodą Halleya miejsca zerowe 
% wielomianu o współczynnikach a zapisanego w bazie wielomianów Czebyszewa 
% pierwszego rodzaju. Zwraca  również informację o tym, ile iteracji było 
% koniecznych do uzyskania wystarczająco dobrego przybliżenia.
% 
%   Argumenty funkcji:
%   x - wektor poziomy zawierający przybliżenia początkowe, w których
%   zaczynamy iteracyjne szukanie miejsc zerowych.
%   a - wektor poziomy współczynników rozważanego wielomianu zapisanego w
%   bazie wielomianów Czebyszewa pierwszego rodzaju
% 
%   Funkcja zwraca :
%   y - wektor o długości length(x), zawierający na i-tym miejscu
%   wartość miejsca zerowego wielomianu o współczynnikach z wektora a
%   zapisanego w bazie wielomianów Czebyszewa pierwszego rodzaju, do którego
%   zbiega w metodzie Halleya ciąg {xk} przy x0 = x(i). Jężeli wyznaczanie
%   miejsca zerowego wymagało więcej niż bądź dokładnie 50 operacji,
%   uznaję, że metoda nie jest zbieżna i zwracam NaN.
%   k - wektor o długości length(x), zawierający na i-tym miejscu
%   informację o tym, ile iteracji było koniecznych, by uzyskać żądane
%   przybliżenie. Jeżeli po 50 iteracjach, wciąż nie spełniony został
%   warunek stopu, zwracamy NaN i uznajemy, że metoda nie jest zbieżna

% Sprawdzenie, czy x jest wektorem poziomym
if ~isrow(x)
    error('Wektor x musi być wektorem poziomym.');
end

% Sprawdzenie, czy a jest wektorem poziomym
if ~isrow(a)
    error('Wektor a musi być wektorem poziomym.');
end

d1 = 0.000001;
y = x;
k = zeros(1, length(x));
kprev = repmat(-1,1,length(x));
while max(k) < 50 && any(kprev ~= k)
    yprev = y;
    kprev = k;
    y = y - wartosciWielomianu(a,y) ./ (wartosciPierwszejPochodnej(a,y) - ...
        (wartosciDrugiejPochodnej(a,y).*(wartosciWielomianu(a,y)./ ...
        (2 .*wartosciPierwszejPochodnej(a,y)))));
    condition = ~(abs(y - yprev) < d1 & abs(y - yprev) < d1 * abs(yprev));
    k = k + condition;
end
for i = 1:length(y)
    if k(i) >=50
        k(i) = NaN;
        y(i) = NaN;
    end
end

end