function [nextIteration] = sorIteration(subDiagonal, mainDiagonal, ...
    supraDiagonal, b, omega, previousIteration)
%SORITERATION Funkcja wykonuje jedną iterację metody SOR (Successive
% OverRelaxation) dla macierzy trójdiagonalnych.
%
%   Argumenty funkcji:
%   - subDiagonal - wektor reprezentujący poddiagonalę macierzy układu
%   (elementy znajdujące się bezpośrednio pod przekątną)
%   - mainDiagonal - wektor reprezentujący diagonalę macierzy układu - jego
%   długość musi być dokładnie o 1 większa od długości wektorów subDiagonal
%   i supraDiagonal
%   - supraDiagonal - wektor reprezentujący naddiagonalę macierzy układu
%   (elementy znajdujące się bezpośrednio nad przekątną)
%   - b - wektor wyrazów wolnych rozwiązywanego układu równań liniowych,
%   musi być równy co do długości wektorowi mainDiagonal
%   - omega - wektor (lub skalar) reprezentujący wszystkie wartości parametru
%   relaksacji, dla których wykonywana jest iteracja
%   - previousIteration - macierz o liczbie wierszy równej długości wektora
%   omega i liczbie kolumn równej długości wektora b, poszczególne wiersze
%   reprezentują wektory uzyskane w poprzedniej iteracji dla danej wartości
%   parametru relaksacji
%
%   Funkcja zwraca:
%   - nextIteration - macierz o wymiarach takich samych jak wymiary
%   macierzy previousIteration, w której każdy wiersz reprezentuje wektor
%   uzyskany w tej iteracji dla danej wartości parametru relaksacji
%   
%   Algorytm został zapisany zgodnie z algorytmem nazwanym w raporcie jako
%   Algorithm 2 'Metoda SOR - wersja 2' (paragraf 1.4)

supraDiagonal = [supraDiagonal, 0];
subDiagonal = [0, subDiagonal];
xPrev = [previousIteration(:,2:end), zeros(length(omega), 1)];
nextIteration = (1 - omega) .* previousIteration + omega .* (b - supraDiagonal.* xPrev) ./ mainDiagonal;
for i = 2:size(nextIteration, 2)
    nextIteration(:, i) = nextIteration(:, i) - (omega ./ mainDiagonal(:, i)) .* (subDiagonal(:,i) .* nextIteration(:,i-1));
end

end 
