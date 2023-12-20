function [tdp] = rekurencyjneWspolczynnikiDrugiejPochodnej(x,n)
%REKURENCYJNEWSPOLCZYNNIKIDRUGIEJPOCHODNEJ Funkcja dla danego wektora
% wejściowego x wyznacza wartości drugiej pochodnej wielomianów Czebyszewa 
% pierwszego rodzaju stopnia od 0 do n w punktach określonych w wektorze x
%   Argumenty funkcji:
%   x - wektor poziomy zawierający punkty, w których chcemy obliczyć
%   wartości drugiej pochodnej wielomianów Czebyszewa pierwszego rodzaju
%   n - maksymalny stopień wielomianu, którego druga pochodną chcemy
%   policzyć
%   Funkcja zwraca :
%   tdp - macierz o rozmiarze n+1 x length(x), zawierającą w i-tej kolumnie
%   i j-tym wierszu wartość drugiej pochodnej wielomianu Czebyszewa 
%   pierwszego rodzaju stopnia j-1 dla i-tej wartości z wektora x.
%   Funkcja w swoich obliczeniach korzysta z funkcji 
%   rekurencyjneWspolczynnikiPierwszejPochodnej(x, n)

% Sprawdzenie, czy x jest wektorem poziomym
if ~isrow(x)
    error('Wektor x musi być wektorem poziomym.');
end

% Sprawdzenie, czy n jest liczbą
if ~isnumeric(n) || ~isscalar(n)
    error('n musi być liczbą.');
end
tpp = rekurencyjneWspolczynnikiPierwszejPochodnej(x, n);
tdp = zeros(n + 1, length(x));
for i = 3: n + 1
    tdp(i,:) = 4 .* tpp(i-1,:) + 2 .* x .* tdp(i-1,:) - tdp(i-2, :);
end
end