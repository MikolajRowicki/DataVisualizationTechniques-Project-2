function [generatedOmega, numberOfIterations, absoluteError, relativeError, ...
    fastestOmega, smallestErrorOmega, accurateResult] = dependenceOfTheNumberOfIterationsOnOmega(subDiagonal, ...
    mainDiagonal,supraDiagonal,b, minOmega, maxOmega, ...
    numberOfOmegas, initialGuess, maxNumberOfIterations, epsilon1, ...
    epsilon2, normType, inApp)
%DEPENDENCEOFTHENUMBEROFITERATIONSONOMEGA Funkcja dokonuje podsumowania
% zarówno zależności szybkości zbieżności metody SOR, jak i błedu
% (bezwzględnego oraz względnego) otrzymanego wyniku od wartości parametru
% relaksacji. Generowane są trzy wykresy - jeden przedstawia zależność 
% liczby iteracji od wartości parametru relaksacji, drugi zależność błędu 
% bezwzględnego od parametru relaksacji, a trzeci zależność błędu względnego 
% od parametru relaksacji. Ponadto zwracana jest wartość parametru relaksacji,
% dla którego metoda w danym przypadku zbiega najszybciej oraz wartość tego
% parametru relaksacji, dla którego uzyskujemy najmniejszy błąd względny,
% szczegóły w dalszej części dokumentacji
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
%   - minOmega - skalar określający minimalną wartość parametru relaksacji
%   - maxOmega - skalar określający maksymalną wartość prametru relaksacji,
%   przy czym maxOmega > minOmega
%   - numberOfOmegas - liczba równomiernie rozłożonych wartości parametru 
%   relaksacji z przedziału [minOmega, maxOmega], dla których wykonywana
%   będzie metoda SOR i narysowany zostanie wykres
%   - initialGuess - wektor reprezentujący przybliżenie początkowe metody
%   SOR o długości równej długości wektora wyrazów wolnych
%   - maxNumberOfIterations - maksymalna liczba iteracji, po przekroczeniu
%   której uznajemy, że dla danej wartości parametru relaksacji metoda nie
%   jest zbieżna (dodatni skalar)
%   - epsilon1 - parametr określający dokładność oznaczony w warunku Gilla
%   jako d1, liczba dodatnia, bliska zeru, ponadto epsilon1 > epsilon2
%   - epsilon2 - parametr określający dokładność oznaczony w warunku Gilla
%   jako d2, liczba dodatnia, bliska zeru, ponadto epsilon1 > epsilon2
%   - normType - w obliczeniach wykorzystywana jest jedna z norm p-tych
%   Schura, przy czym normType (dodatni skalar lub 'Inf' lub '-Inf')
%   okresla wartość współczynnika p
%   - inApp - logiczny skalar informujący, czy funkcja jest wywoływana
%   wewnątrz aplikacji 'aplikacja'(wówczas argument ten ma wartość 'true'), czy
%   też w dowolnym miejscu poza nią
%
%   ! Uwaga ! Wektory przyjmowane jako argumenty funkcji mogą być podawane
%   zarówno w konwencji pionowej, jak i poziomej, funkcja dostosuje je do
%   swoich potrzeb
%
%   Funkcja zwraca:
%   - generatedOmega - wektor poziomy reprezentujący wszystkie rozważane wartości
%   parametru relaksacji z przedziału [minOmega, maxOmega]; jeżeli
%   wygenerowana zostanie wartość parametru relaksacji równa 0, jest ona
%   usuwana z wektora omega; wektor ten ma zatem w domyślnym przypadku
%   długość równą wartości parametru numberOfOmegas lub numberOfOmegas - 1,
%   gdy trzeba usunąć 0
%   - numberOfIterations - wektor poziomy o długości równej wektorowi omega
%   reprezentujący liczbę iteracji potrzebnych do uzyskania wyniku o żądanej
%   dokładności dla danej wartości parametru relaksacji, gdy metoda nie
%   jest zbieżna dla parametru relaksacji stojącego na i-tym miejscu w
%   wektorze omega, to k(i) = NaN
%   - absoluteError - wektor poziomy o długości równej wektorowi omega
%   reprezentujący błąd bezwzględny wyniku uzyskanego metodą SOR przy danej
%   wartości parametru relaksacji
%   - relativeError - wektor poziomy o długości równej wektorowi omega
%   reprezentujący błąd względny wyniku uzyskanego metodą SOR przy danej
%   wartości parametru relaksacji
%   - fastestOmega - skalar reprezentujący wartość parametru relaksacji, dla
%   którego można najszybciej uzyskać rozwiązanie z żądaną dokładnością, dla
%   tej wartości parametru relaksacji wykonywanych jest najmniej iteracji
%   - smallestErrorOmega - skalar reprezentujący wartość parametru relaksacji,
%   dla którego można uzyskać najdokładniejsze rozwiązanie pod kątem błędu
%   względnego
%   - accurateResult - wektor poziomy reprezentujący dokładne rozwiązanie
%   rozważanego układu równań liniowych, uzyskany metodą wbudowaną w
%   MATLABA (wykorzystywana jest tu metoda GEPP)

if ~isscalar(minOmega) || ~isnumeric(minOmega) 
    error('minOmega musi być liczbą.');
end

if ~isscalar(maxOmega) || ~isnumeric(maxOmega) 
    error('maxOmega musi być liczbą.');
end

if ~isscalar(numberOfOmegas) || ~isnumeric(numberOfOmegas) || numberOfOmegas <= 0 || mod(numberOfOmegas, 1) ~= 0
    error('numberOfOmegas musi być całkowitą liczbą większą od 0.');
end

if minOmega >= maxOmega
    error('minOmega musi być mniejsze niż maxOmega.');
end

if ~isscalar(inApp) || ~islogical(inApp)
    error('inApp musi być logicznym skalarem')
end

% Tworzenie wektora omega według zadanych warunków
generatedOmega = linspace(minOmega,maxOmega,numberOfOmegas);
generatedOmega = generatedOmega(generatedOmega ~= 0);
% Wykonanie metody SOR
[result,numberOfIterations, fastestOmega] = sorMethod(subDiagonal,mainDiagonal,supraDiagonal,b,generatedOmega, ...
    initialGuess, maxNumberOfIterations, epsilon1, epsilon2, normType);
result(~(numberOfIterations<=maxNumberOfIterations)) = NaN;

% Obliczenie błędów
[absoluteError, relativeError, smallestErrorOmega, accurateResult] = computeError(subDiagonal, mainDiagonal, supraDiagonal, b, generatedOmega, result, normType);

% Wygenerowanie wykresów w sytuacji, gdy metoda nie jest wywoływana w
% aplikacji
if ~inApp
    figure

    % Generowanie wykresu zależności liczby iteracji od parametru relaksacji
    subplot(2,2,1)
    plot(generatedOmega, numberOfIterations, 'LineWidth', 2);
    axis([minOmega maxOmega 0 maxNumberOfIterations])
    title('Zależność liczby iteracji od parametru relaksacji');
    xlabel('Parametr relaksacji')
    ylabel('Liczba iteracji')
    hold on;

    % Generowanie wykresu zależności błędu bezwzględnego od parametru relaksacji
    subplot(2,2,2) 
    semilogy(generatedOmega, absoluteError, 'LineWidth', 2);
    axis([minOmega maxOmega 0 inf])
    title('Zależność błędu bezwzględnego od parametru relaksacji')
    xlabel('Parametr relaksacji')
    ylabel('Wartość błedu bezwzględnego wyniku')

    % Generowanie wykresu zależności błędu względnego od parametru relaksacji
    subplot(2,2,3)
    semilogy(generatedOmega, relativeError, 'LineWidth', 2);
    axis([minOmega maxOmega 0 inf])
    title('Zależność błędu względnego od parametru relaksacji')
    xlabel('Parametr relaksacji')
    ylabel('Wartość błedu względnego wyniku')
end

end