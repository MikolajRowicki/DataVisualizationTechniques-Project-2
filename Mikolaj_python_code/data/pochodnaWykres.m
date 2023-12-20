function [] = pochodnaWykres()
%UNTITLED Funkcja generuje okno graficzne z trzema wykresami
%   W lewym górnym rogu generowany jest wykres funkcji func(x).
%   W lewym dolnym rogu generowany jest wykres funkcji pochodnaD().
%   Po prawej stronie generowany jest wykres funkcji pochodnaP() dla
%   różnych n = [-2, -6, -10 , -15, -20], gdzie h = 2^n

% Tworzenie wykresów
x = linspace(0, 2*pi);
n = [-2,-6,-10,-15,-20];
h = 2.^n;
figure;
subplot(2, 2, 1);
plot(x, func(x), 'r', 'LineWidth', 2);
title('Funkcja f(x) = 2 * cos(x/2)', 'FontSize',14);
xlabel('x');
ylabel('Wartość funkcji');

subplot(2, 2, 3);
plot(x, pochodnaD(x), 'g', 'LineWidth', 2);
title('Pochodna dokładna funkcji f(x) = 2 * cos(x/2)', 'FontSize',14);
xlabel('x');
ylabel('Wartość funkcji');

subplot(2, 2, [2,4]);
plot(x, pochodnaP(x,h(1,1), @func));
hold on;
plot(x, pochodnaP(x,h(1,2), @func));
hold on;
plot(x, pochodnaP(x,h(1,3), @func));
hold on;
plot(x, pochodnaP(x,h(1,4), @func));
hold on;
plot(x, pochodnaP(x,h(1,5), @func));
hold on;
title('Pochodna przybliżona funkcji f(x) = 2 * cos(x/2)', 'FontSize', 14);
xlabel('x');
ylabel('Wartość funkcji');
legend('n = -2', 'n = -6', 'n = -10', 'n = -15', 'n = -20');

end 

