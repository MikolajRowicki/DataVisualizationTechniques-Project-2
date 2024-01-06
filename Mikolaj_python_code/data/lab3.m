% x=5;y=2;
% 
% z=exfun(x,y);
% k=exfunF(x,y,@exfun);
% 
% figure
% figure(2)
% figure(60)
% close all;
% figure(1);plot(1:10,1:10);hold on;plot(20:30,20:30)
% close all;
% figure(1);plot(1:10,1:10, 'mo');hold on;plot(20:30,20:30)
% close all;
% figure(3)
% subplot(2,2,1)
% plot(1:10,1:10,'r')
% subplot(2,2,4)
% plot(0:0.1:100,0:0.1:100,'*')
% subplot(2,2,2)
% loglog(0:1000, 0:1000)
% theta = 0:0.01:2*pi;
% rho =sin(2*theta)
% close all;
% figure
% figure(2)
% figure(60)
% close all;
% figure(1);plot(1:10,1:10);hold on;plot(20:30,20:30)
% close all;
% figure(1);plot(1:10,1:10, 'mo');hold on;plot(20:30,20:30)
% close all;
% figure(3)
% subplot(2,2,1)
% plot(1:10,1:10,'r')
% subplot(2,2,4)
% plot(0:0.1:100,0:0.1:100,'*')
% subplot(2,2,2)
% loglog(0:1000, 0:1000)
% theta = 0:0.01:2*pi;
% rho =sin(2*theta)
% close all;
% 
% figure(3)
% plot(1:10,1:10,'r')
% title('tytul wykresu')
% legend('wartość 1')
% xlabel('nazwa_x')
% ylabel('nazwa_y')
% grid off;
% axis([0 5 0 5])
% text(5,5,'tekst')

% Zadanie 1
n = [-2,-6,-10,-15,-20];
h = 2.^n
x = pi/6;
pochodnaP(x,h,@func)

[W, Z] = pochodnaTable(x,@func,@pochodnaD,@pochodnaP,n);

colnames = {'n', 'pochodna dokładna', 'pochodna przybliżona'}
W = table(W(:,1), W(:,2), W(:,3), 'VariableNames', colnames)

x = linspace(0, 2 * pi)
n = -12

[W, Z] = pochodnaTable(x,@func,@pochodnaD,@pochodnaP,n);

colnames = {'x', 'pochodna dokładna', 'pochodna przybliżona'}
Z = table(Z(:,1), Z(:,2), Z(:,3), 'VariableNames', colnames)

% Zadanie 2
pochodnaWykres()

