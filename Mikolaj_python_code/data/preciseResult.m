function accurateResult = preciseResult(subDiagonal, mainDiagonal, supraDiagonal, b)
%PRECISERESULT Funkcja wyznacza dokładne rozwiązanie układu równań z
% macierzą trójdiagonalną, wykorzystując wbudowaną w MATLABA metodę GEPP,
% zwracany wektor przedstawiany jest w orientacji poziomej.
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
%
%   ! Uwaga ! Wektory przyjmowane jako argumenty funkcji mogą być podawane
%   zarówno w konwencji pionowej, jak i poziomej, funkcja dostosuje je do
%   swoich potrzeb
%
%   Funkcja zwraca:
%   - accurateResult - wektor poziomy reprezentujący dokładne rozwiązanie
%   rozważanego układu równań liniowych, uzyskany metodą wbudowaną w
%   MATLABA (wykorzystywana jest tu metoda GEPP)

% Sprawdzenie warunków
if ~(isvector(subDiagonal) || isempty(subDiagonal))|| ~isvector(mainDiagonal) || ~(isvector(supraDiagonal) || isempty(supraDiagonal)) || ~isvector(b)
    error('subDiagonal, mainDiagonal, supraDiagonal, b i omega muszą być wektorami.');
end

if length(mainDiagonal) ~= length(subDiagonal) + 1 || length(mainDiagonal) ~= length(supraDiagonal) + 1
    error('mainDiagonal musi być długości o 1 większej niż subDiagonal i supraDiagonal.');
end

if length(b) ~= length(mainDiagonal)
    error('b musi być wektorem o długości równej mainDiagonal.');
end

if any(mainDiagonal == 0)
    error('na diagonali macierzy ukladu nie może być 0')
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

% Stworzenie macierzy w oparciu o podaną pod-, nad- i zwykłą diagonalę
A = full(spdiags([[subDiagonal, 0]', mainDiagonal', [0, supraDiagonal]'], ...
    -1:1, length(mainDiagonal), length(mainDiagonal)));

% Obliczenie dokładnego wyniku
accurateResult = A \ b';
accurateResult = accurateResult';

end