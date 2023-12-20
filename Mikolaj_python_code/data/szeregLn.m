function [Y] = szeregLn(x, Nmax)
%SZEREGLN Funkcja wyznacza rozwnięcie funkcji ln(x+1) w szereg potęgowy.
%   Funkcja przyjmuje następujące argumenty:
%   Wejście: x - wektor wejściowe
%   Nmax – maksymalna liczba wyrazów rozwinięcia funkcji w szereg (wtedy
%          we wzorze jest suma od n=0 do n=Nmax-1)
%   Wyjście: macierz Y, której wiersze to wartości funkcji ln(x+1) 
%             wyznaczone dla wektora x i dla N = {1,2,3,....,Nmax}.
XMat = repmat(x,Nmax,1);
NMat = repmat((0:1:Nmax-1)', 1, length(XMat));
Y = cumsum(((XMat .^(NMat + 1)) .*((-1).^NMat)) ./ factorial(NMat + 1) , 1);
end