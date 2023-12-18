function [] = wizualizacjaZbieznosci(wielomian,a,b,c,d,n,m)
% WIZUALIZACJAZBIEZNOSCI Funkcja dla danego wektora wejściowego oraz 
% liczb a,b,c,d,n,m tworzy wizualizację zbieżności metody Halleya, a także
% - dla pierwiastków rzeczywistych - tworzy wizualizację pokazująca, do
% jakich pierwiastków zbiega ciąg zapoczątkowany przez dane przybliżenie
% początkowe.
%   Argumenty funkcji:
%   wielomian - wektor poziomy współczynników rozważanego wielomianu zapisanego w
%   bazie wielomianów Czebyszewa pierwszego rodzaju
%   a - minimalna wartość części rzeczywistej liczb, dla których tworzona 
%   jest wizualizacja, musi być mniejsza od b
%   b - maksymalna wartość części rzeczywistej liczb, dla których tworzona 
%   jest wizualizacja, musi być większa od a
%   c - minimalna wartość części urojonej liczb, dla których tworzona 
%   jest wizualizacja, musi być mniejsza od d
%   d - maksymalna wartość części urojonej liczb, dla których tworzona 
%   jest wizualizacja, musi byc większa od c
%   n - pomniejszona o 1 liczba punktów z przedziału[a,b] włącznie z
%   krańcami, dla których będzie obliczać wartości funkcji metodaHalleya,
%   musi byc dodatnia
%   m - pomniejszona o 1 liczba punktów z przedziału[c,d] włącznie z
%   krańcami, dla których będzie obliczać wartości funkcji metodaHalleya
%   Funkcja w swoich obliczeniach korzysta z funkcji,
%   musi być dodatnia.
%   metodaHalleya. Jeżeli dany wielomian ma wyłącznie rzeczywiste
%   pierwiastki, generowany są dwa wykresy. Jeden z nich pokazuje szybkość
%   zbieżności metody Halleya dla każdego punktu początkowego na rozważanym
%   wycinku płaszczyzny zespolonej. Drugi natomiast pokazuje, do jakiego
%   pierwiastka zbiega ciąg xk, przy ustalonym x0. W sytuacji, gdy mamy do
%   czynienia z wielomianem o pierwiastkach zespolonych, generowany jest
%   wyłącznie wykresz szybkości zbieżności metody Halleya.

% Sprawdzenie warunków dla wektorów i liczb
    if ~isnumeric(a) || ~isscalar(a) || ~isnumeric(b) || ~isscalar(b) || ~isnumeric(c) || ~isscalar(c) || ~isnumeric(d) || ~isscalar(d) || ~isnumeric(m) || ~isscalar(m) || ~isnumeric(n) || ~isscalar(n)
        error('Wszystkie parametry muszą być liczbami skalarnymi.');
    end
    
    % Sprawdzenie warunków dla wektorów
    if ~isrow(a) || ~isrow(c)
        error('a i c muszą być wektorami poziomymi.');
    end

    % Sprawdzenie warunków dodatkowych
    if b <= a
        error('b musi być większe od a.');
    end

    if d <= c
        error('d musi być większe od c.');
    end

    if n <= 0 || m <= 0
        error('n i m muszą być liczbami dodatnimi.');
    end

x = a:(b-a)/n:b;
y = c:(d-c)/m:d;

% Macierz A przechowuje informację o tym, do jakiego pierwiastka zbiega
% ciąg xk przy danym przybliżeniu początkowym.
A = zeros(n+1, m+1);

% Macierz A przechowuje informację o szybkości zbieżności metody Halleya
% przy danym przybliżeniu początkowym.

B = zeros(n+1, m+1);
for k = 1:m+1
        [p,q] = metodaHalleya(x + y(k) * 1i, wielomian);            
        A(k,:) = p;
        B(k,:) = q;
end

roots = dokladneWartosci(wielomian);

% Wykresu wyświetlam zarówno w bazowym formacie zapewnionym przez MATLAB,
% jak i z wykorzystaniem pakietu plotly, by zachować interaktywność i móc
% wyświetlać dokładne wartości pierwiastków, do których zbiega ciąg
% przybliżeń zapocątkowany w danym punkcie oraz liczby operacji.

figure
if imag(roots) == zeros(1,length(roots))
    subplot(1,2,1);
    imagesc([a b], [c d] ,B);
    colorbar; 
    set(gca, 'YDir', 'normal');
    xlabel('Re');
    ylabel('Im');
    colormap jet;

    title('Liczba iteracji')
    
    
    subplot(1,2,2);
    imagesc([a b], [c d], real(A));
    colorbar;
    colormap jet;
    xlabel('Re');
    ylabel('Im');

    title('Pierwiastki wielomianu');
    set(gca, 'YDir', 'normal');
    set(gcf, 'Position', [100, 100, 1000, 300]);
    
    %Ta funkcja pozwala wyświetlić wykresy w postaci interaktywnej,
    %wykorzystując bibliotekę plotly, zachęcam do odkomentowania i
    %sprawdzenia (uwaga! możliwe, że będzie konieczna instalacja tego
    %pakietu)

    %fig2plotly();
    
    

else
    imagesc([a b], [c d] ,B);
    colorbar; 
    colormap jet;
    set(gca, 'YDir', 'normal');
    % Dodanie podpisów osi
    xlabel('Re');
    ylabel('Im');

    title('Liczba iteracji')

    %Ta funkcja pozwala wyświetlić wykresy w postaci interaktywnej,
    %wykorzystując bibliotekę plotly, zachęcam do odkomentowania i
    %sprawdzenia (uwaga! możliwe, że będzie konieczna instalacja tego
    %pakietu)

    %fig2plotly();


end