function [result, numberOfIterations, fastestOmega] = sorMethod(subDiagonal, mainDiagonal, ...
    supraDiagonal, b, omega, initialGuess, maxNumberOfIterations, epsilon1, epsilon2, normType)
%SORMETHOD Funkcja implementuje iteracyjny algorytm metody SOR, opisany w
% raporcie pod nazwą Algorithm 2 'Metoda SOR - wersja 2'

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
%   - initialGuess - wektor reprezentujący przybliżenie początkowe metody
%   SOR o długości równej długości wektora wyrazów wolnych b
%   - maxNumberOfIterations - maksymalna liczba iteracji, po przekroczeniu
%   której uznajemy, że dla danej wartości parametru relaksacji metoda nie
%   jest zbieżna (dodatni skalar)
%   - epsilon1 - parametr określający dokładność oznaczony w warunku Gilla
%   jako d1, liczba dodatnia, bliska zeru, ponadto epsilon1 > epsilon2
%   - epsilon2 - parametr określający dokładność oznaczony w warunku Gilla
%   jako d2, liczba dodatnia, bliska zeru, ponadto epsilon1 > epsilon2
%   - normType - w obliczeniach wykorzystywana jest jedna z norm p-tych
%   Schura, przy czym normType (dodatni skalar lub 'Inf' lub '-Inf')
%   określa wartość współczynnika p
%
%   ! Uwaga ! Wektory przyjmowane jako argumenty funkcji mogą być podawane
%   zarówno w konwencji pionowej, jak i poziomej, funkcja dostosuje je do
%   swoich potrzeb
%
%   Funkcja zwraca:
%   - result - macierz, której poszczególne wiersze reprezentują rozwiązania 
%   rozważanego układu równań liniowych uzyskane metodą SOR dla odpowiednich
%   wartości parametru relaksacji; liczba kolumn musi być równa długości
%   wektora mainDiagonal, zaś liczba wierszy musi być równa długości
%   wektora omega; wektor reprezentujący i-ty wiersz macierzy result
%   oznacza wynik uzyskany metodą SOR o parametrze relaksacji równym
%   omega(i)
%   - numberOfIterations - wektor (poziomy) o długości równej wektorowi omega,
%   reprezentujący liczbę iteracji potrzebnych do uzyskania rozwiązania o
%   zadowalającej dokładności dla danej wartości parametru relaksacji
%   - fastestOmega - skalar reprezentujący wartość parametru relaksacji, dla
%   którego można najszybciej uzyskać rozwiązanie z dobrą dokładnością, dla
%   tej wartości parametru relaksacji wykonywanych jest najmniej iteracji


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

if ~isscalar(epsilon1) || ~isnumeric(epsilon1) || epsilon1 <= 0
    error('epsilon1 musi być liczbą większą od 0.');
end

if ~isscalar(epsilon2) || ~isnumeric(epsilon2) || epsilon2 <= 0
    error('epsilon2 musi być liczbą większą od 0.');
end

if epsilon2 >= epsilon1
    error('zgodnie z warunkiem Gilla epsilon1 musi byc większy niż epsilon2')
end

if all(omega == 0)
    error('omega musi być różna od 0')
end

if any(mainDiagonal == 0)
    error('na diagonali macierzy ukladu nie może być 0')
end

if ~((isscalar(normType) && isnumeric(normType) && normType > 0) || normType == Inf || normType == -Inf)
    error('normType musi reprezentować wbudowany w MATLAB-a typ normy Schura');
end

% Zamiana wektorów na odpowiednią orientację
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

omega = omega(omega ~= 0);
result = initialGuess;
prevX = initialGuess;
numberOfIterations = zeros(1,size(result,1));

for i = 1:maxNumberOfIterations
    result = sorIteration(subDiagonal, mainDiagonal, supraDiagonal, b, omega, result);  

    % Obliczenie różnic między odpowiadającymi sobie wierszami x i prevX
    differences = result - prevX;

    % Obliczenie normy dla każdego wiersza
    norms = vecnorm(differences', normType)';
    normPrevX = vecnorm(prevX', normType)';

    % Sprawdzenie, czy spełniony jest warunek Gilla dla wszystkich wartości
    % parametru relaksacji
    if all(norms < epsilon1 * normPrevX + epsilon2)
        break;
    end
    
    % Aktualizacja liczby iteracji tylko dla tych wartości parametru relaksacji,
    % dla których nie jest jeszcze spełniony warunek Gilla
    numberOfIterations(norms >= epsilon1 * normPrevX + ...
        epsilon2) = numberOfIterations(norms >= epsilon1* normPrevX + epsilon2) + 1;

    prevX = result;
end

numberOfIterations(numberOfIterations == maxNumberOfIterations) = NaN;

for i = 1:size(result,2)
numberOfIterations(~isfinite(result(:, i))) = NaN;
end

% Ustalenie najlepszej wartości parametru relaksacji
if isscalar(omega)
    fastestOmega = omega;
else
    [~, minIndex] = min(numberOfIterations);
    fastestOmega = omega(minIndex);
end  

end