function [Z] = szeregBlad(x,Nmax)
%szeregBlad wyznacza zależność błędu względnego pomiędzy wartościami
%dokładnymi y funkcji sin(x) a wartościami yaprox w zależności od wartości
%paramteru N dla dowolnie wybranej wartości argumentu x
Z = zeros(Nmax, 5)

for N = 1:Nmax
    
x_wybrane = randi(length(x)) * pi / 100;
blad = (func(x_wybrane) - szeregSin(x_wybrane, N))/x_wybrane;

Z(N,:) = [N,x_wybrane,szeregSin(x_wybrane, N), func(x_wybrane), blad];

end
end