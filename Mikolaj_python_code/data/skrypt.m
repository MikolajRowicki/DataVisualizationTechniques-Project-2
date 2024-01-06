% Celem poniższego skrytpu jest wygenerowanie sześciu przykładów opisanych
% w sprawozdaniu prezentujących działanie zaimplementowanej przeze mnie 
% metody SOR. Poniższy kod pozwala na wygenerowanie większości wykresów i
% tabeli użytych w sprawozdaniu, choć część z nich to zrzuty ekranu z 
% aplikacji. Część z poniższych wyników nie zostało zawartych w
% sprawozdaniu, stanowią one jednak jedynie uzupełnienie głównych
% rezultatów badania.

% Uwaga! Wywołanie całego skryptu naraz może zająć nawet do kilkunastu
% minut. Warto uruchamiać pojedyncze sekcje.

%% PRZYKŁAD 1

% Zdefiniujmy macierz układu i wektor wyrazów wolnych, a także przybliżenie
% początkowe

subDiagonal1 = [95.5167, 0.7726, 27.1334, 6.5482];
mainDiagonal1 = [44.7319, 69.1642, 72.7636, 55.6798, 22.6542];
supraDiagonal1 = [0.9631, 41.9198, 5.5193, 17.2809];
b1 = [9.8476, 95.9317, 51.35, 33.3126, 11.1257];
initialGuess1 = zeros(1,5);
[~,~,~,~,fastestOmega1,smallestErrorOmega1,accurateResult1] = dependenceOfTheNumberOfIterationsOnOmega(subDiagonal1,...
    mainDiagonal1,supraDiagonal1,b1, 0, 2, 1000, initialGuess1, 5000, power(10,-10), ...
    power(10, -20), 1, false);
norm1 = norm(accurateResult1, 1);
radius1a = spectralRadius(subDiagonal1, mainDiagonal1, supraDiagonal1, 1.033);
radius1b = spectralRadius(subDiagonal1, mainDiagonal1, supraDiagonal1, 0.2);
[result1,~,~] = sorMethod(subDiagonal1, mainDiagonal1, supraDiagonal1, b1, 1.033, initialGuess1, ...
    5000, power(10,-10), power(10, -20), 1);

%% PRZYKŁAD 2

% Zdefiniujmy macierz układu i wektor wyrazów wolnych, a także przybliżenie
% początkowe

subDiagonal2 = 0.9999;
mainDiagonal2 = [10, 0.7];
supraDiagonal2 = 7;
b2 = [17, 1.7];
initialGuess2 = zeros(1,2);
[~,~,~,~,fastestOmega2,smallestErrorOmega2,accurateResult2] = dependenceOfTheNumberOfIterationsOnOmega(subDiagonal2,...
    mainDiagonal2,supraDiagonal2, b2, 0, 2, 1000, initialGuess2, 5000, power(10,-10), ...
    power(10, -20), 1, false);
A2 = full(spdiags([[subDiagonal2, 0]', mainDiagonal2', [0, supraDiagonal2]'], ...
    -1:1, length(mainDiagonal2), length(mainDiagonal2)));
detA2 = det(A2);
condA2 = cond(A2,1);
[result2,~,~] = sorMethod(subDiagonal2, mainDiagonal2, supraDiagonal2, b2, 1.98198, initialGuess2, ...
    5000, power(10,-10), power(10, -20), 1);
radius2a = spectralRadius(subDiagonal2, mainDiagonal2, supraDiagonal2, 1.98198);
radius2b = spectralRadius(subDiagonal2, mainDiagonal2, supraDiagonal2, 1);

%% PRZYKŁAD 3

% Zdefiniujmy macierz układu i wektor wyrazów wolnych, a także przybliżenie
% początkowe

subDiagonal3 = [-184.1339, 222.9813, -657.6641, 361.153];
mainDiagonal3 = [-906.6028, -74.0064, 617.3838, 928.9155, -744.1802];
supraDiagonal3 = [-3.8829, -152.0178, 974.202, -116.8637];
b3 = [-223.1074, -529.8207, 417.3093, 753.428, -331.1455];
initialGuess3 = zeros(1,5);
[~,~,~,~,fastestOmega3,smallestErrorOmega3,accurateResult3] = dependenceOfTheNumberOfIterationsOnOmega(subDiagonal3,...
    mainDiagonal3,supraDiagonal3, b3, 0, 2, 1000, initialGuess3, 5000, power(10,-10), ...
    power(10, -20), 2, false);
radius3a = spectralRadius(subDiagonal3, mainDiagonal3, supraDiagonal3, 0.9189);
radius3b = spectralRadius(subDiagonal3, mainDiagonal3, supraDiagonal3, 2);
radius3c = spectralRadius(subDiagonal3, mainDiagonal3, supraDiagonal3, 1.42);
[result3,~,~] = sorMethod(subDiagonal3, mainDiagonal3, supraDiagonal3, b3, 0.9189, initialGuess3, ...
    5000, power(10,-10), power(10, -20), 2);

%% PRZYKŁAD 4

% Zdefiniujmy macierz układu i wektor wyrazów wolnych, a także przybliżenie
% początkowe

subDiagonal4 = [0.7377, 2.5353, 6.6895, 1.8978];
mainDiagonal4 = [2.0817, 3.7082, 9.6104, 8.2526, 1.5397];
supraDiagonal4 = [0.7377, 2.5353, 6.6895, 1.8978];
b4 = [9.7543, 9.6608, 7.0553, 6.7692, 3.6936];
initialGuess4a = zeros(1,5);
[~,~,~,~,fastestOmega4a,smallestErrorOmega4a,accurateResult4a] = dependenceOfTheNumberOfIterationsOnOmega(subDiagonal4,...
    mainDiagonal4,supraDiagonal4, b4, 0, 2, 1000, initialGuess4a, 2000, power(10,-10), ...
    power(10, -20), 1, false);
initialGuess4b = ones(1,5);
[~,~,~,~,fastestOmega4b,smallestErrorOmega4b,accurateResult4b] = dependenceOfTheNumberOfIterationsOnOmega(subDiagonal4,...
    mainDiagonal4,supraDiagonal4, b4, 0, 2, 1000, initialGuess4b, 2000, power(10,-10), ...
    power(10, -20), 1, false);
[result4,~,~] = sorMethod(subDiagonal4, mainDiagonal4, supraDiagonal4, b4, 1.7918, initialGuess4a, ...
    2000, power(10,-10), power(10, -20), 1);
radius4a = spectralRadius(subDiagonal4, mainDiagonal4, supraDiagonal4, 1.7918);
radius4b = spectralRadius(subDiagonal4, mainDiagonal4, supraDiagonal4, 0.984985);
radius4c = spectralRadius(subDiagonal4, mainDiagonal4, supraDiagonal4, 0.938939);

%% PRZYKŁAD 5

% Zdefiniujmy macierz układu i wektor wyrazów wolnych, a także przybliżenie
% początkowe

subDiagonal5 = ones(1,9999);
mainDiagonal5 = 1:10000;
supraDiagonal5 = ones(1,9999);
b5 = 1:10000;
initialGuess5 =  zeros(1, 10000);

[~,~,~,~,fastestOmega5,smallestErrorOmega5,accurateResult5] = dependenceOfTheNumberOfIterationsOnOmega(subDiagonal5,...
    mainDiagonal5,supraDiagonal5, b5, 0, 2, 1000, initialGuess5, 2000, power(10,-10), ...
    power(10, -20), 1, false);

[result5,~,~] = sorMethod(subDiagonal5, mainDiagonal5, supraDiagonal5, b5, fastestOmega5, initialGuess5, ...
    2000, power(10,-10), power(10, -20), 1);

Time5a = timeit(@() sorMethod(subDiagonal5, mainDiagonal5, supraDiagonal5, b5, fastestOmega5, initialGuess5, 2000, power(10,-10), power(10,-20), 1), 3);
% Time5a = 0.0077

Time5b = timeit(@() preciseResult(subDiagonal5, mainDiagonal5, supraDiagonal5,b5), 1);
% Time5b = 9,0277

%% PRZYKŁAD 6

% Zdefiniujmy macierz układu i wektor wyrazów wolnych, a także przybliżenie
% początkowe

subDiagonal6 = ones(1,99999);
mainDiagonal6 = 1:100000;
supraDiagonal6 = ones(1,99999);
b6 = 1:100000;
initialGuess6 =  zeros(1, 100000);

% Poniżej generowany jest wykres zależności liczby iteracji od parametru
% relaksacji. Zdefiniowana w tym celu metoda
% dependenceOfTheNumberOfIterationsOnOmega niestety nie działa dla tak
% dużych macierzy, gdyż zawarte jest w niej obliczenie błędu, które w tym
% przypadku jest niemożliwe.

[result6,numberOfIterations6,fastestOmega6] = sorMethod(subDiagonal6, mainDiagonal6, supraDiagonal6, b6, 0.002:0.002:2, initialGuess6, ...
    2000, power(10,-10), power(10, -20), 1);
plot(0.002:0.002:2, numberOfIterations6, 'LineWidth', 2);
axis([0 2 0 2000])
title('Zależność liczby iteracji od parametru relaksacji');
xlabel('Parametr relaksacji')
ylabel('Liczba iteracji')
hold on;

Time6a = timeit(@() sorMethod(subDiagonal6, mainDiagonal6, supraDiagonal6, b6, fastestOmega6, initialGuess6, 1000, power(10,-10), power(10,-20), 1), 3);
% Time6a = 0.0451

% Po wywołaniu poniższej funkcji uzyskujemy następujący komunikat: 
% 'Requested 100000x100000 (74.5GB) array exceeds maximum array size 
% preference (7.8GB). This might cause MATLAB to become unresponsive.'
% Tego układu nie da się zatem rozwiązać wbudowań w MATLABA metodą GEPP.

% Nie odkomentowywać !! 
% Time6b = timeit(@() preciseResult(subDiagonal6, mainDiagonal6, supraDiagonal6,b6));
% Time6b
