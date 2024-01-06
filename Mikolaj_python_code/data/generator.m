function [subDiagonal, mainDiagonal, supraDiagonal] = generator(n, scalarOmega, hasToBeSymmetric, minValue, maxValue)
%GENERATOR Funkcja generuje wektory odpowiadające kolejno poddiagonali, diagonali i
% naddiagonali macierzy układu A o elementach z rozkładu jednostajnego na
% przedziale (minValue, maxValue). Wektory generowane są w taki sposób, że
% wyznaczona za ich pomocą macierz iteracji B_SOR ma promień spektralny
% mniejszy niż 1.
%
%   Argumenty funkcji:
%   - n - dodatni, całkowity skalar reprezentujący wymiar macierzy układu,
%   czyli także długość wektora mainDiagonal
%   - scalarOmega - różny od 0 skalar reprezentujący wartość parametru
%   relaksacji, dla którego tworzona będzie macierz iteracji
%   - hasToBeSymmetric - logiczny skalar, decydujący o tym, czy
%   wygenerowana macierz musi być symetryczna (wówczas wartość tego
%   parametru jest ustawiona na true)
%   - minValue - skalar określający dolny kraniec przedziału, z którego
%   losowane będą elementy macierzy
%   - maxValue = skalar określający górny kraniec przedziału, z którego
%   losowane będą elementy macierzy
%
%   Funkcja zwraca:
%   - subDiagonal - wektor poziomy reprezentujący poddiagonalę macierzy układu
%   (elementy znajdujące się bezpośrednio pod przekątną)
%   - mainDiagonal - wektor poziomy reprezentujący diagonalę macierzy układu - jego
%   długość musi być dokładnie o 1 większa od długości wektorów subDiagonal
%   i supraDiagonal
%   - supraDiagonal - wektor poziomy reprezentujący naddiagonalę macierzy układu
%   (elementy znajdujące się bezpośrednio nad przekątną)
%
%   ! Uwaga ! Ze względu na fakt, że rzadko macierze o dużych rozmiarach
%   mają promień spektralny < 1, zalecane jest korzystanie z tej funkcji
%   dla n < 20. Dla n >= 20 czas oczekiwania na wygenerowanie odpowiednich
%   wektorów może być bardzo długi.

% Sprawdzenie warunków
if ~isscalar(n) || ~isnumeric(n) || n <= 0 || mod(n, 1) ~= 0
    error('n musi być całkowitą liczbą większą od 0.');
end

if ~isscalar(scalarOmega) || ~isnumeric(scalarOmega) 
    error('maxOmega musi być liczbą.');
end

if scalarOmega == 0
    error('omega musi być różna od 0')
end

if ~isscalar(hasToBeSymmetric) || ~islogical(hasToBeSymmetric)
    error('hasToBeSymmetric musi być logicznym skalarem')
end

if ~isscalar(minValue) || ~isnumeric(minValue) 
    error('minValue musi być liczbą.');
end

if ~isscalar(maxValue) || ~isnumeric(maxValue) 
    error('maxValue musi być liczbą.');
end

if minValue >= maxValue
    error('minValue musi być mniejsze niż maxValue.');
end

radius = 'Inf';

while radius >= 1 
    mainDiagonal = (maxValue - minValue) * rand(1,n) + minValue;
    if hasToBeSymmetric
        subDiagonal = (maxValue - minValue) * rand(1,n-1) + minValue;
        supraDiagonal = subDiagonal;
    else
        subDiagonal = (maxValue - minValue) * rand(1,n-1) +  minValue;
        supraDiagonal = (maxValue - minValue) * rand(1,n-1) + minValue;
    end
    radius = spectralRadius(subDiagonal, mainDiagonal, supraDiagonal, scalarOmega);
end

end