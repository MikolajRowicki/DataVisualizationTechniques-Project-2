function [root] = dokladneWartosci(a)
% DOKLADNEWARTOSCI Funkcja dla danego wektora wejściowego wyznacza dokładne
% wartości miejsc zerowych wielomianów zapisanych w bazie wielomianów 
% Czebyszewa pierwszego rodzaju o współczynnikach z wektora a, korzystając
% w tym celu z obliczeń symbolicznych.
%   Argumenty funkcji:
%   a - wektor poziomy współczynników rozważanego wielomianu zapisanego w
%   bazie wielomianów Czebyszewa pierwszego rodzaju
%   Funkcja zwraca :
%   root - wektor poziomy dokładnych miejsc zerowych wielomianu o 
%   współczynnikach a zapisanego w bazie wielomianów Czebyszewa pierwszego
%   rodzaju. 

% Sprawdzenie, czy a jest wektorem poziomym
if ~isrow(a)
    error('Wektor a musi być wektorem poziomym.');
end

syms x
n = 0:1:length(a)-1;
chebyszevPolymonial= chebyshevT(n,x);
w=a .* chebyszevPolymonial;
polymonial = sum(w);
coefficients  = double(coeffs(polymonial, 'all'));
root = roots(coefficients);
end