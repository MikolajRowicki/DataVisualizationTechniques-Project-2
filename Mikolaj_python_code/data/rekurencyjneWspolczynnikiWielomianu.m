function [t] = rekurencyjneWspolczynnikiWielomianu(x, n)
% REKURENCYJNEWSPOLCZYNNIKIWIELOMIANU Funkcja dla danego wektora
% wejściowego x wyznacza wartości wielomianów Czebyszewa pierwszego rodzaju
% stopnia od 0 do n w punktach określonych w wektorze x
%   Argumenty funkcji:
%   x - wektor poziomy zawierający punkty, w których chcemy obliczyć
%   wartości wielomianów Czebyszewa pierwszego rodzaju
%   n - maksymalny stopień wielomianu, którego wartość chcemy
%   policzyć
%   Funkcja zwraca :
%   t - macierz o rozmiarze n+1 x length(x), zawierającą w i-tej kolumnie
%   i j-tym wierszu wartość wielomianu Czebyszewa 
%   pierwszego rodzaju stopnia j-1 dla i-tej wartości z wektora x.
%   Funkcja wylicza wartości wielomianu Czebyszewa pierwszego rodzaju w
%   danym punkcie, wykorzystując definicję rekurencyjną.

% Sprawdzenie, czy x jest wektorem poziomym
if ~isrow(x)
    error('Wektor x musi być wektorem poziomym.');
end

% Sprawdzenie, czy n jest liczbą
if ~isnumeric(n) || ~isscalar(n)
    error('n musi być liczbą.');
end

t = zeros(n + 1, length(x));
t(1,:) = 1;
t(2,:) = repmat(x,1);
for i = 3: n + 1
    t(i,:) = 2 .* x .* t(i-1,:) - t(i-2,:);
end

end