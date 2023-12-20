function [yaprox] = szeregSin(x, Nmax)
% szeregSin wyznacza przybliżoną wartość funkcji sin(x)
% x - wektor agumentów
% Nmax - maksymalna liczba wyrazów rozwinięcia funkcji
%domyślna wartość Nmax = 9

if nargin < 1
    error("Za mało argumentów")
elseif nargin == 1
    Nmax = 9;
elseif nargin > 2
    error("Za dużo argumentów")
end

yaprox_mat = zeros(Nmax+1, length(x));

for k = 0:Nmax
    yaprox_mat(k+1,:)=((-1)^k).*(x.^(2*k+1))/factorial(2*k+1);
end

yaprox = sum(yaprox_mat, 1);

end