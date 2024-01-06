function [y] = myhorner(c,x,t)

%MYHORNER Funkcja dla zadanych wektorów c, x oraz t oblicza uogólnionym 
% algorytmem Hornera wartości wielomianu w punktach t, gdzie
% c – współczynniki wielomianu w postaci Newtona
% x - węzły
% t - wektor argumentów

%   Funkcja zwraca wektor wynikowy y, zawierający wartości wielomianu,
%   którego wymiary są takie same jak wektora wejściowego t.

%   Wektory przyjmowane przez funkcję jako argumenty muszą być wektorami
%   poziomymi.

if  size(c, 1) ~= 1 || size(x, 1) ~= 1 || size(t, 1) ~= 1
    error(['Nieprawidłowe dane wejściowe: wektory wejściowe muszą być' ...
        ' poziome']);
end

%   Generuję wektor wynikowy y  o wartościach równych c(length(c)) i
%   wymiarach wektora t

y = repmat(c(length(c)), 1, length(t));

%   Obliczm wartości wielomianu w punktach t, korzystając z uogólnionego
%   schematu Hornera.

for k = length(c)-1:-1:1
    y = c(k) + (t-x(k)).*y;
end

end