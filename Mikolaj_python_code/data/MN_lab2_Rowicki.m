% 2a)
x = 0:pi/100:2*pi;
y = func(x);

% 2b)
x = 0:pi/100:2*pi;
yaprox = szeregSin(x, 10)

% 2c)
x = 0:pi/100:2*pi;
Z = szeregBlad(x, 10)

plot(x,y)

% 2d)
colnames = {'N','x','yaprox','yreal','blad'};
c = table(Z(:,1), Z(:,2), Z(:,3), Z(:,4), Z(:,5), 'VariableNames', colnames)