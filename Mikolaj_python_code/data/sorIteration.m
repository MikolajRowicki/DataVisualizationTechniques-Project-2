function [nextIteration] = sorIteration(subDiagonal, mainDiagonal, ...
    supraDiagonal, b, omega, previousIteration)
%SORITERATION Funkcja wykonuje jedną iterację metody SOR (Successive
% OverRelaxation) dla macierzy trójdiagonalnych.
%   Argumenty funkcji:
%   - subDiagonal - wektor reprezentujący poddiagonalę macierzy układu
%   (elementy znajdujące się bezpośrednio pod przekątną)
%   - mainDiagonal - wektor reprezentujący diagonalę macierzy układu - jego
%   długość musi być dokładnie o 1 większa od długości wektorów subDiagonal
%   i supraDiagonal
%   - supraDiagonal - wektor reprezentujący naddiagonalę macierzy układu
%   (elementy znajdujące się bezpośrednio nad przekątną)

supraDiagonal = [supraDiagonal, 0];
subDiagonal = [0, subDiagonal];
xPrev = [previousIteration(:,2:end), zeros(length(omega), 1)];
nextIteration = (1 - omega) .* previousIteration + omega .* (b - supraDiagonal.* xPrev) ./ mainDiagonal;
for i = 2:size(nextIteration, 2)
    nextIteration(:, i) = nextIteration(:, i) - (omega / mainDiagonal(:, i)) .* (subDiagonal(:,i) .* nextIteration(:,i-1));
end

end