% Skrypt prezentuje działanie funkcji rozklad(a,b) wyznaczającej rozkład 
% Banachiewicza-Cholesky-ego dodatnio określonej, symetrycznej, 
% trójprzekątniowej macierzy A

% Przykład 1
a = [1,5,5,5];
b = [2,2,2];

[d, s] = rozklad(a,b);

% d = [1, 1, 1, 1]
% s = [2, 2, 2]

% Przykład 2
a = [5,4,7];
b = [1,2];

[d, s] = rozklad(a, b);

% d = [2.2361, 1.9494, 2.4387]
% s = [0.4472, 1.0260]

% Przykład 3
a = [5, 4, 7, 6, 5]
b = [1, 2, 4, 2]

[d, s] = rozklad(a, b)

% d = [2.2361, 1.9494, 2.4387, 1.8193, 1.9472]
% s = [0.4472, 1.0260, 1.6402, 1.0993]
