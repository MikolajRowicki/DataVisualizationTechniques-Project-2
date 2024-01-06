% pages.mini.pw.edu.pl/~jasionowskam
% u: student
% p: mniad23Z

% nieskończoność
inf
Inf

%zmienne w środku funkcji nie są wypisywane
a = 5
b = 6

whos

clear all;
close all;

calc_b(10)

% zamiast definiować funkcje na końcu skryptu można też stosować wyrażenia
% inline

f1 = inline('cos(2*x*pi)');
y = f1(0.5);

x = [5,6,0,9]
any(x)
find(x)

eye(5)
find(ans)

A = eye(5)
[r c] = find(A)

function [b] = calc_b(a)
    if a > 0
        b = a + 5;
    else
        b = a - 5;
    end
end
