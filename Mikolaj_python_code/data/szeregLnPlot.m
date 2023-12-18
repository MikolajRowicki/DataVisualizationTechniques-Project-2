function szeregLnPlot(x, Nmax, f)
% SZEREGLNPLOT Funkcja generuje okno graficzne z wykresem funkcji ln(x + 1)
% i NMax wykresami kolejnych przybliżeń tej funkcji
    
figure;
colormap(parula(Nmax)); % ustawienie różnych kolorów dla wykresów
cmap = colormap;
plot3(x, log(1 + x), zeros(length(x)), 'b-', 'LineWidth', 2);
hold on;

% Rysowanie przybliżeń dla różnych N
Y = f(x, Nmax);
for N = 1:Nmax 
    color = cmap(N, :);
    plot3(x, Y(N, :), repmat(N,1,length(x)), 'Color', color, 'LineStyle', ...
        '--', 'LineWidth', 0.5);
end
hold on;
grid on;

% Ustawienia wykresu
xlabel('Wektor wejściowy x');
ylabel('Wartości funkcji');
zlabel('Kolejne wykresy');
title('Rozwinięcie funkcji ln(x+1) w szereg potęgowy');
view([-37.5, 30]); % ustawienie widoku trójwymiarowego
hold off;
end
