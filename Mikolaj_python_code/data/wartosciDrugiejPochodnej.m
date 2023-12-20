function [dp] = wartosciDrugiejPochodnej(a,x)
%WARTOSCIDRUGIEJPOCHODNEJ Funkcja dla danych wektorów wejściowych x i a 
% wyznacza wartości drugiej pochodnej wielomianów zapisanych w bazie 
% wielomianów Czebyszewa pierwszego rodzaju o współczynnikach z wektora a w
% w punktach określonych w wektorze x
%   Argumenty funkcji:
%   x - wektor poziomy zawierający punkty, w których chcemy obliczyć
%   wartości drugiej pochodnej wielomanów o współczynnikach z wektora a 
%   zapisanych w bazie wielomianów Czebyszewa pierwszego rodzaju
%   a - wektor poziomy współczynników rozważanego wielomianu zapisanego w
%   bazie wielomianów Czebyszewa pierwszego rodzaju
%   Funkcja zwraca :
%   dp - wektor o długości length(x), zawierający na i-tym miejscu
%   wartość drugiej pochodnej wielomianu o współczynnikach z wektora a
%   zapisanego w bazie wielomianów Czebyszewa pierwszego rodzaju w punkcie 
%   x(i).
%   Funkcja w swoich obliczeniach korzysta z funkcji 
%   rekurencyjneWspolczynnikiDrugiejPochodnej(x, n), wykorzystując fakt, że
%   przy ustalonym x poszukiwany wielomian to standardowy iloczyn skalarny
%   wektora współczynników i wektora drugich pochodnych kolejnych stopniem
%   wielomianów Czebyszewa pierwszego rodzaju w punkcie x

% Sprawdzenie, czy x jest wektorem poziomym
if ~isrow(x)
    error('Wektor x musi być wektorem poziomym.');
end

% Sprawdzenie, czy a jest wektorem poziomym
if ~isrow(a)
    error('Wektor a musi być wektorem poziomym.');
end

n = length(a) - 1;
tdp = rekurencyjneWspolczynnikiDrugiejPochodnej(x, n);
dp = a * tdp;
end