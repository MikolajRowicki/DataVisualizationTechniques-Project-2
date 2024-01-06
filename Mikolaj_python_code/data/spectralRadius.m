function radius = spectralRadius(subDiagonal, mainDiagonal, supraDiagonal, scalarOmega)
%SPECTRALRADIUS Funkcja wylicza promień spektralny macierzy iteracji B_SOR
% zdefiniowanej w sprawozdaniu na stronie 6 (paragraf 1.4) danej
% następującym wzorem: B_SOR = (D + omega*L)^(-1) * (D*(1-omega) - omega*U),
% gdzie macierz D to macierz diagonalna o elementach z wektora mainDiagonal,
% macierz U ma niezerowe elementy na naddiagonali i są to elementy z
% wektora supraDiagonal, macierz L ma niezerowe elementy tylko na
% poddiagonali i są to elementy z wektora subDiagonal, zaś omega jest
% argumentem funkcji
%
%   Argumenty funkcji:
%   - subDiagonal - wektor reprezentujący poddiagonalę macierzy układu
%   (elementy znajdujące się bezpośrednio pod przekątną)
%   - mainDiagonal - wektor reprezentujący diagonalę macierzy układu - jego
%   długość musi być dokładnie o 1 większa od długości wektorów subDiagonal
%   i supraDiagonal
%   - supraDiagonal - wektor reprezentujący naddiagonalę macierzy układu
%   (elementy znajdujące się bezpośrednio nad przekątną)
%   - scalarOmega - różny od 0 skalar reprezentujący wartość parametru
%   relaksacji, dla którego tworzona będzie macierz iteracji
%
%   Funkcja zwraca: 
%   - radius - nieujemny skalar reprezentujący promień spektralny, czyli
%   moduł największej co do modułu wartości własnej macierzy iteracji B_SOR

% Sprawdzenie warunków
if ~(isvector(subDiagonal) || isempty(subDiagonal))|| ~isvector(mainDiagonal) || ~(isvector(supraDiagonal) || isempty(supraDiagonal)) 
    error('subDiagonal, mainDiagonal, supraDiagonal, b i omega muszą być wektorami.');
end

if length(mainDiagonal) ~= length(subDiagonal) + 1 || length(mainDiagonal) ~= length(supraDiagonal) + 1
    error('mainDiagonal musi być długości o 1 większej niż subDiagonal i supraDiagonal.');
end

if ~isscalar(scalarOmega) || ~isnumeric(scalarOmega) 
    error('maxOmega musi być liczbą.');
end

if scalarOmega == 0
    error('omega musi być różna od 0')
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

%  Wygenerowanie macierzy D, L i U
size_B_SOR = length(mainDiagonal);
D = full(spdiags(mainDiagonal', 0, size_B_SOR, size_B_SOR));
L = full(spdiags(subDiagonal', -1, size_B_SOR, size_B_SOR));
U = full(spdiags([0,supraDiagonal]', 1, size_B_SOR, size_B_SOR));

% Wyznaczenie macierzy B_SOR = inv(D + omega*L) * (D*(1-omega) - omega*U)
B_SOR = (D + scalarOmega*L) \ (D*(1-scalarOmega) - scalarOmega*U);

% Obliczenie wartości własnych macierzy B_SOR
eigenvalues_B_SOR = eig(B_SOR);

% Wybranie maksymalnej wartości własnej
radius = max(abs(eigenvalues_B_SOR));

end