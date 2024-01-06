% x = linspace(1,100);
% y = sin(x);
% % 
%  figure(100)
% subplot(3,2,1)
% bar3(x,y)
% subplot(3,2,2)
 % stairs(x,y)
% subplot(3,2,3)
% stem(x,y)
% subplot(3,2,4)
% pie([1,2,3,4,1,2,3,4,5])
% subplot(3,2,5)
 % pie3([1,2,3,4,1,2,3,4,5]) % wykres kołowy 3d
% subplot(3,2,6)
% area(x,[y;2*y]') %wykres warstwowy

% figure(150)
% % hist(y,20)
% hist(y,[-1,0,0.5,1])

%% animacja
% 
% h = animatedline;
% axis([0,4*pi,-1,1])
% 
% numpoints = 100000;
% x = linspace(0,4*pi, numpoints);
% y = sin(x);
% for k = 1:numpoints
%     addpoints(h,x(k),y(k))
%     drawnow limitrate
% end

% x = [-15:0.5:15];
% y = -15:0.5:15;
% [X,Y] = meshgrid(x,y);
% R = sqrt(X.^2+Y.^2);

clear all;

%Zad 1
x = linspace(-0.5,0.5,10000);

Nmax = 7;

[y] = szeregLn(x,Nmax);

szeregLnPlot(x,Nmax, @szeregLn); 
hold on;
colormap(parula(Nmax));
cmap = colormap;

line1 = animatedline('Color', cmap(1,:), 'LineStyle', "--", 'LineWidth', 1.5);
line2 = animatedline('Color', cmap(2,:), 'LineStyle', "--", 'LineWidth', 1.5);
line3 = animatedline('Color', cmap(3,:), 'LineStyle', "--", 'LineWidth', 1.5);
line4 = animatedline('Color', cmap(4,:), 'LineStyle', "--", 'LineWidth', 1.5);
line5 = animatedline('Color', cmap(5,:), 'LineStyle', "--", 'LineWidth', 1.5);
line6 = animatedline('Color', cmap(6,:), 'LineStyle', "--", 'LineWidth', 1.5);
line7 = animatedline('Color', cmap(7,:), 'LineStyle', "--", 'LineWidth', 1.5);

for i = 1:100: length(x)
    addpoints(line1, x(i), y(1,i), 1);
    addpoints(line2, x(i), y(2,i), 2);
    addpoints(line3, x(i), y(3,i), 3);
    addpoints(line4, x(i), y(4,i), 4);
    addpoints(line5, x(i), y(5,i), 5);
    addpoints(line6, x(i), y(6,i), 6);
    addpoints(line7, x(i), y(7,i), 7);

    drawnow limitrate;
    pause(0.00001)

end

