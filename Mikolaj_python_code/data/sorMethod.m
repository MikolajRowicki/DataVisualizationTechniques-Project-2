function [prevX, numberOfIterations] = sorMethod(subDiagonal, mainDiagonal, ...
    supraDiagonal, b, omega, initialGuess, maxNumberOfIterations, epsilon)
%UNTITLED4 Summary of this function goes here
%   Detailed explanation goes here

% Sprawdzenie warunków
if ~(isvector(subDiagonal) || isempty(subDiagonal))|| ~isvector(mainDiagonal) || ~(isvector(supraDiagonal) || isempty(supraDiagonal)) || ~isvector(b) || ~isvector(omega)
    error('subDiagonal, mainDiagonal, supraDiagonal, b i omega muszą być wektorami.');
end

if length(mainDiagonal) ~= length(subDiagonal) + 1 || length(mainDiagonal) ~= length(supraDiagonal) + 1
    error('mainDiagonal musi być długości o 1 większej niż subDiagonal i supraDiagonal.');
end

if length(b) ~= length(mainDiagonal)
    error('b musi być wektorem o długości równej mainDiagonal.');
end

if ~isscalar(maxNumberOfIterations) || ~isnumeric(maxNumberOfIterations) || maxNumberOfIterations <= 0 || mod(maxNumberOfIterations, 1) ~= 0
    error('maxNumberOfIterations musi być całkowitą liczbą większą od 0.');
end

if size(initialGuess, 2) ~= length(mainDiagonal)
        error('Liczba kolumn macierzy initialGuess musi być równa długości wektora mainDiagonal.');
end

if ~isscalar(epsilon) || ~isnumeric(epsilon) || epsilon <= 0
    error('epsilon musi być liczbą większą od 0.');
end

if any(omega == 0)
    error('omega musi być różna od 0')
end

if any(mainDiagonal == 0)
    error('na diagonali macierzy ukladu nie może być 0')
end

% Zamień wektory na odpowiednią orientację if-ami
if size(subDiagonal, 1) > 1
    subDiagonal = subDiagonal';
end

if size(mainDiagonal, 1) > 1
    mainDiagonal = mainDiagonal';
end

if size(supraDiagonal, 1) > 1
    supraDiagonal = supraDiagonal';
end

if size(b, 1) > 1
    b = b';
end

if size(omega, 2) > 1
    omega = omega';
end

if length(omega) ~= size(initialGuess, 1)
    initialGuess = repmat(initialGuess, length(omega), 1);
end 

x = initialGuess;
prevX = initialGuess;
numberOfIterations = zeros(1,size(x,1));

for i = 1:maxNumberOfIterations
    x = sorIteration(subDiagonal, mainDiagonal, supraDiagonal,b, omega, x);  

    % Oblicz normę pierwszą z różnicy dla każdego wiersza jednocześnie
    norms = sum(abs(x - prevX), 2);
    % Sprawdź, czy wszystkie sumy są mniejsze niż próg
    if all(norms < epsilon)
        break;
    end
    
    % Aktualizuj liczbę iteracji tylko dla tych, które nie spełniają warunku
    numberOfIterations(norms >= epsilon) = numberOfIterations(norms >= epsilon) + 1;

    prevX = x;
end

numberOfIterations(numberOfIterations == maxNumberOfIterations) = NaN;

for i = 1:size(x,2)
numberOfIterations(~isfinite(x(:, i))) = NaN;
end

end