function [] = wielowykres(f,a,b,n)

%WIELOWYKRES Funkcja dla zadanych argumentów f, a, b, n, gdzie:
% f – funkcja interpolowana
% a, b – końce przedziału
% n - maksymalny stopień wielomianu interpolacyjnego
% rysuje na jednym obrazku wykres funkcji f, jej wielomianu 
% interpolacyjnego, a także wyraźnie zaznacza punkty (xk, f(xk)) – gdzie 
% xk to wyznaczone węzły (równoodległe). 

%   Oś y wykresu jest wyskalowania względem wartości funkcji f: od 
%   2*fmin − fmax do 2*fmax − fmin, gdzie fmin i fmax oznaczają 
%   przybliżenia, odpowiednio, najmniejszej i największej wartości funkcji 
%   f na przedziale [a, b]. Oś x wykresu jest wyskalowana od a do b.

%   Wyznaczam n równoodległych punktów (węzłów interpolacji) z przedziału
%   [a,b].

wezly = linspace(a,b,n);

%   Obliczam wartości funkcji w tych węzłach.

wartosci_wezlow = f(wezly);

%   Wyznaczam wektor współczynników postaci Newtona wielomianu 
%   interpolacyjnego spełniającego warunki p(wezly) = wartosci_wezlow.
%   Współczynnikiem stałym (wolnym) wielomianu jest c(1).

c = ilorazr(wezly, wartosci_wezlow);

%   Generuję punkty x na osi OX do rysowania wykresu. 

x = linspace(a,b,1000);

%   Wyznaczam wartości wielomianu interpolacyjnego w punktach x.

y_approximated_plot = myhorner(c,wezly,x);

%   Wyznaczam dokładne wartości interpolowanej funckji f w punktach x.

y_plot = f(x);

%   Wyznaczam minimalną i maksymalną wartość przyjmowaną przez funkcję f w
%   rozważanym przedziale.

fmin = min(y_plot);
fmax = max(y_plot);

figure;hold on;

%   Tworzę wykres interpolowanej funkcji f.

plot(x, y_plot, 'b', 'LineWidth', 0.5, 'DisplayName', 'f(x)');

%   Tworzę wykres wielomianu interpolacyjnego.

plot(x, y_approximated_plot,'r--', 'LineWidth', 0.5, 'DisplayName', ...
    'Interpolating Polynomial');

%   Zaznaczam na wykresie węzły interpolacji i wartości funkcji f w tych
%   węzłach.

plot(wezly,wartosci_wezlow, 'go', 'MarkerSize', 3, 'MarkerFaceColor', ...
    'g', 'DisplayName', 'Interpolation Points');

%   Skaluję odpowiednio osie OX i OY.
axis([a, b, 2*fmin - fmax, 2 * fmax - fmin]);

%Dodaję podpisy osi i tytuł

xlabel('x');
ylabel('y');
title('Interpolation of f(x)');
legend('Location', 'Best');
grid on;
hold off;
end
