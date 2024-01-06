function [resultMatrix, vectorWithNorms] = sprawdzBlad(a,x,y,k)
% DOKLADNEWARTOSCI Funkcja wyznacza błąd bezwzględny metody Halleya dla
% danego wielomianu i danych przybliżeń początkowych. Funkcja oblicza normy
% p-te z wektora różnicy dokładnych miejsc zerowych i tych uzyskanych
% metodą Halleya
%   Argumenty funkcji:
%   a - wektor poziomy współczynników rozważanego wielomianu zapisanego w
%   bazie wielomianów Czebyszewa pierwszego rodzaju
%   x - wektor poziomy przybliżeń początkowych
%   y - wektor poziomy miejsc zerowych wyznaczonych za pomocą metody
%   Halleya, gdzie y(i) jest miejscem zerowym wyznaczonym dla przybliżenia
%   początkowego równego x(i)
%   k - liczba iteracji dokonanych w metodzie Halleya, gdzie k(i) oznacza
%   liczbę iteracji dla przybliżenia początkowego równego x(i).
%   Funkcja zwraca :
%   resultMatrix - macierz o liczbie wierszy równej length(x) i liczbie 
%   kolumn równej 5. Nazwy poszczególnych kolumn :  'Przybliżenie 
%   początkowe','Miejsca zerowe znalezione metodą Halleya', 'Dokładne 
%   miejsca zerowe', 'Liczba iteracji metody Halleya', 'Błąd bezwzględny 
%   wyniku'. W i-tym wierszu możemy sprawdzić wartość wszystkich tych
%   parametrów dla i-tego przybliżenia początkowego.
%   vectorWithNorms - wektor poziomy o długości 4,który oblicza normy
%   p-te z wektora różnicy dokładnych miejsc zerowych i tych uzyskanych
%   metodą Halleya. Odpowiednio są to normy dla p = 1,2,1000, 'inf'


root = dokladneWartosci(a);


% Inicjalizacja macierzy wynikowej
resultMatrix = zeros(length(y), 5);
vectorWithNorms = zeros(1, 4);

% Iteracja przez elementy wektora x
for i = 1:length(y)
    % Znajdź indeks najbliższego elementu w wektorze root
    [~, index] = min(abs(root - y(i)));
    
    % Zapisz wyniki w macierzy wynikowej
    resultMatrix(i, 1) = x(i);
    resultMatrix(i, 2) = y(i);
    if isnan(resultMatrix(i, 2))
        resultMatrix(i, 3) = NaN;
    else
        resultMatrix(i, 3) = root(index);
    end
    resultMatrix(i, 4) = k(i);
    resultMatrix(i, 5) = abs(resultMatrix(i,2) - resultMatrix(i,3));

end

vectorWithNorms(1) = norm(resultMatrix(:,3) - resultMatrix(:,2), 1);
vectorWithNorms(2) = norm(resultMatrix(:,3) - resultMatrix(:,2), 2);
vectorWithNorms(3) = norm(resultMatrix(:,3) - resultMatrix(:,2), 1000);
vectorWithNorms(4) = norm(resultMatrix(:,3) - resultMatrix(:,2), "inf");

colnames = {'Przybliżenie początkowe','Miejsca zerowe znalezione metodą Halleya', 'Dokładne miejsca zerowe', 'Liczba iteracji metody Halleya', 'Błąd bezwzględny wyniku'};
resultMatrix = table(resultMatrix(:,1), resultMatrix(:,2), resultMatrix(:,3),resultMatrix(:,4),resultMatrix(:, 5), 'VariableNames', colnames);


colnames = {'Norma 1', 'Norma 2', 'Norma 1000', 'Norma inf'};
vectorWithNorms= table(vectorWithNorms(1), vectorWithNorms(2), vectorWithNorms(3),vectorWithNorms(4), 'VariableNames', colnames);

end