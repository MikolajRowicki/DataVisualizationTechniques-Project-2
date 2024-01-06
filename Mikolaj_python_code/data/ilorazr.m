function c = ilorazr(x,y)
% c - tablica (wektor) współczynników postaci Newtona
% wielomianu interpolacyjnego spełniającego warunki
% p(x) = y. Współczynnikiem stałym (wolnym) wielomianu
% jest c(1).
n = length(y);
c = y;
for i = 2:n
    c(i:n) = ( c(i:n) - c(i-1:n-1) ) ./ ( x(i:n) - x(1:n-i+1) );
end
end