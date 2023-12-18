function [d, s] = rozklad(a,b)

%ROZKLAD Funkcja wyznacza rozkład Banachiewicza-Cholesky-ego dodatnio 
% określonej, symetrycznej, trójprzekątniowej macierzy A, (gdzie 
% A = A*(L^T))

%   Funkcja przyjmuje wektor wejściowy a, który jest wektorem wartości
%   znajdujących się na przekątnej macierzy A  oraz wektor b, który zawiera
%   wartości znajdujące się powyżej głównej przekątnej (i tym samym poniżej
%   niej, bo macierz jest symetryczna). Funkcja zwraca wektor d - wektor 
%   wartości na przekątnej macierzy L i wektor s - wektor wartości 
%   znajdujących się pod przekątną macierzy L.

%   Wektory wejściowe muszą być poziome, ponadto długośc wektora a musi być
%   o 1 większa od długości wektora b. W przeciwnym przypadku wyrzucam
%   error. Nie sprawdzam, czy z wektorów wejściowych powstanie nam macierz
%   dodatnio określona. Zakładam, że użytkownik poda odpowiednie

if ~isvector(a) || ~isvector(b) || length(a) ~= length(b) + 1 || size(a, 1) ~= 1 || size(b, 1) ~= 1
    error(['Nieprawidłowe dane wejściowe: a powinno być wektorem poziomym' ...
        ' o długości o 1 większej niż b.']);
end

n = length(a);
d = zeros(1, n);
s = zeros(1, n - 1);

%   Tworzę macierz A, która na przekątnej głównej ma wartości z wektora A, 
%   zaś nad nią i pod nią ma wartości z wektora b.

A = diag(a) + diag(b, 1) + diag(b, -1);

%   Wyliczam pierwsze wartości wektorów d i s według zoru

d(1) = sqrt(A(1, 1));
s(1) = A(1, 2) / d(1);

% Pozostałe elementy wyliczam, korzystając z faktu, że dla wszystkich 
% niezerowych elementów występujących w macierzy A |i - j| <= 1, co
% oznacza, że wiele wyrazów występujących w ogólnym wzorze rozkładu
% Banachiewicza-Cholesky-ego będzie zerami, które można opisać. Uproszczone
% algorytmy przedstawiają się następująco:

for i = 2:n-1
    d(i) = sqrt(A(i, i) - s(i-1) ^ 2);
    s(i) = A(i + 1, i) / d(i);
end

d(n) = sqrt(A(n, n) - s(n - 1) ^ 2);

end