function [absoluteError, relativeError, smallestErrorOmega, accurateResult] = computeError(subDiagonal, mainDiagonal, supraDiagonal, b, omega, result, normType)
%COMPUTEERROR Funkcja wylicza błąd względny i bezwzględny rozwiązania
% pewnego układu równań liniowych dla zadanych wartości parametru
% relaksacji. Zwracane jest również dokładne rozwiązanie układu oraz
% wartość parametru relaksacji, dla której otrzymujemy wynik obarczony
% najmniejszym błędem względnym
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
%   - result - macierz, której poszczególne wiersze reprezentują rozwiązania 
%   rozważanego układu równań liniowych uzyskane metodą SOR dla odpowiednich
%   wartości parametru relaksacji; liczba kolumn musi być równa długości
%   wektora mainDiagonal, zaś liczba wierszy musi być równa długości
%   wektora omega; wektor reprezentujący i-ty wiersz macierzy result
%   oznacza wynik uzyskany metodą SOR o parametrze relaksacji równym
%   omega(i)
%   - normType - w obliczeniach wykorzystywana jest jedna z norm p-tych
%   Schura, przy czym normType (dodatni skalar lub 'Inf' lub '-Inf')
%   okresla wartość współczynnika p
%
%   ! Uwaga ! Wektory przyjmowane jako argumenty funkcji mogą być podawane
%   zarówno w konwencji pionowej, jak i poziomej, funkcja dostosuje je do
%   swoich potrzeb
%
%   Funkcja zwraca:
%   - absoluteError - wektor poziomy o długości równej wektorowi omega
%   reprezentujący błąd bezwzględny wyniku uzyskanego metodą SOR przy danej
%   wartości parametru relaksacji
%   - relativeError - wektor poziomy o długości równej wektorowi omega
%   reprezentujący błąd względny wyniku uzyskanego metodą SOR przy danej
%   wartości parametru relaksacji
%   - smallestErrorOmega - skalar reprezentujący wartość parametru relaksacji,
%   dla którego można uzyskać najdokładniejsze rozwiązanie pod kątem błędu
%   względnego
%   - accurateResult - wektor poziomy reprezentujący dokładne rozwiązanie
%   rozważanego układu równań liniowych, uzyskany metodą wbudowaną w
%   MATLABA (wykorzystywana jest tu metoda GEPP)

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

if size(result, 2) ~= length(mainDiagonal)
        error('Liczba kolumn macierzy result musi być równa długości wektora mainDiagonal.');
end

if size(result, 1) ~= length(omega)
        error('Liczba wierszy macierzy result musi być równa długości wektora omega.');
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

% Obliczenie dokładnego wyniku
accurateResult = preciseResult(subDiagonal, mainDiagonal, supraDiagonal, b);

% Obliczenie błędu bezwzględnego
absoluteError = vecnorm(result - repmat(accurateResult, size(result, 1), 1), normType, 2)';

% Obliczenie błędu względnego
relativeError = absoluteError ./ vecnorm(repmat(accurateResult, size(result, 1), 1) , normType, 2)';

% Znalezienie parametru relaksacji, zapewniającego najmniejszy błąd
% względny wyniku
if isscalar(relativeError)
    smallestErrorOmega = relativeError;
else
    [~, minIndex] = min(relativeError);
    smallestErrorOmega = omega(minIndex);
end  

end