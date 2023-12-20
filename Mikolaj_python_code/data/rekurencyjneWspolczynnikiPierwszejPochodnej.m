function [tpp] = rekurencyjneWspolczynnikiPierwszejPochodnej(x,n)
%REKURENCYJNEWSPOLCZYNNIKIPIERWSZEJPOCHODNEJ Funkcja dla danego wektora
% wejściowego x wyznacza wartości pierwszej pochodnej wielomianów Czebyszewa 
% pierwszego rodzaju stopnia od 0 do n w punktach określonych w wektorze x
%   Argumenty funkcji:
%   x - wektor poziomy zawierający punkty, w których chcemy obliczyć
%   wartości pierwszej pochodnej wielomianów Czebyszewa pierwszego rodzaju
%   n - maksymalny stopień wielomianu, którego pierwszą pochodną chcemy
%   obliczyć
%   Funkcja zwraca :
%   tpp - macierz o rozmiarze n+1 x length(x), zawierającą w i-tej kolumnie
%   i j-tym wierszu wartość pierwszej pochodnej wielomianu Czebyszewa 
%   pierwszego rodzaju stopnia j-1 dla i-tej wartości z wektora x.
%   Funkcja w swoich obliczeniach korzysta z funkcji 
%   rekurencyjneWspolczynnikiWielomianu(x, n)

% Sprawdzenie, czy x jest wektorem poziomym
if ~isrow(x)
    error('Wektor x musi być wektorem poziomym.');
end

% Sprawdzenie, czy n jest liczbą
if ~isnumeric(n) || ~isscalar(n)
    error('n musi być liczbą.');
end
t = rekurencyjneWspolczynnikiWielomianu(x, n);
tpp = zeros(n + 1, length(x));
tpp(2,:) = 1;
for i = 3: n + 1
    tpp(i,:) = 2 .* t(i-1,:) + 2 .* x .* tpp(i - 1, :) - tpp(i - 2, :);
end
end