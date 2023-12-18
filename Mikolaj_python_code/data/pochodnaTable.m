function [W, Z] = pochodnaTable(x,f,pd,pp,n)
%POCHODNATABLE Funkcja przyjmuje liczbę lub wektor (x), funkcję
%różniczkowalną f , a także funkcję będącą dokłądną pochodną f (pd) 
% i przybliżoną pochodną f (pp) i zwraca macierze W oraz Z
%   W - macierz rozmiaru length(h) x 3, której kolumny to: wektor wartości
%   n, wektor wartości pierwszej pochodnej dokładnej, wektor wartości
%   pierwszej pochodnej przybliżonej
%   Z - macierz rozmiaru length(x) x 3, której kolumny to: wektor wartości
%   x, wektor wartości pierwszej pochodnej dokładnej, wektor wartości
%   przybliżenia pierwszej pochodnej
h = 2.^n;
W = zeros(length(h), 3);
Z = zeros(length(x), 3);
if length(x) == 1 & length(n) ~= 1
W = [n', repmat(pd(x), length(h), 1), pp(x,h, @func)'];
else
Z = [x', pd(x)', pp(x,h,@func)];
end
end