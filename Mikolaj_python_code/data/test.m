% mainDiagonal = [1000000,2,2];
% subDiagonal = [-1,-1];
% supraDiagonal = [-1,-1];
% b = [6,8,15];
% omega = 1.2;
% initialGuess =  zeros(1,3);
% [x,k] = sorMethod(subDiagonal, mainDiagonal, supraDiagonal, b, omega, ...
%     initialGuess);

% mainDiagonal = [4,4,4];
% subDiagonal = [-1,-1];
% supraDiagonal = [-1,-1];
% b = [2,6,2];
% % omega = [1/2,1/2, 1/2, 3/2, 1.2,100];
% initialGuess =  zeros(1,3);
% % [x,k] = sorMethod(subDiagonal, mainDiagonal, supraDiagonal, b, omega, ...
% %     initialGuess);
% dependenceOfTheNumberOfIterationsOnOmega(subDiagonal,mainDiagonal, supraDiagonal, b, ...
%     -5, 10, 1000, initialGuess, 1000, power(10,-8))

% 
% mainDiagonal = [4,4,4];
% subDiagonal = [-1,-1];
% supraDiagonal = [-1,-1];
% b = [2,6,2];
% initialGuess =  zeros(1,3);
% dependenceOfTheNumberOfIterationsOnOmega(subDiagonal,mainDiagonal, supraDiagonal, b, initialGuess)

% 
% 
% mainDiagonal = [4,4,4];
% subDiagonal = [-1,-1];
% supraDiagonal = [-1,-1];
% b = [2,6,2];
% initialGuess =  [-10,2,5];
% dependenceOfTheNumberOfIterationsOnOmega(subDiagonal,mainDiagonal, supraDiagonal, b, initialGuess)



% mainDiagonal = [4,4,4,4,4,4,4];
% subDiagonal = [1,1,1,1,1,1];
% supraDiagonal = [1,1,1,1,1,1];
% b = [2,6,2, 22,4,11,2];
% initialGuess =  [0,0,0,0,0,0,0];
% [y,k] = sorMethod(subDiagonal, mainDiagonal, supraDiagonal, b, 0.5 , initialGuess, ...
%     1000, power (10, -4));
%  dependenceOfTheNumberOfIterationsOnOmega(subDiagonal,mainDiagonal, supraDiagonal, b, -10,10, ...
%     1000, initialGuess, 1000, power(10,-10))



% mainDiagonal = 10 *rand(1,6);
% subDiagonal = 10 * rand(1,5);
% supraDiagonal = 10 * rand(1,5);
% b = [-27,42,56,48,92,122];
% initialGuess =  zeros(1,6);
% [x,k] = sorMethod(subDiagonal, mainDiagonal, subDiagonal, b, [-5,-2], initialGuess, 1000,power(10,-10));
% 
% 
% dependenceOfTheNumberOfIterationsOnOmega(subDiagonal,mainDiagonal, supraDiagonal, b, -10,10,1000,initialGuess, ...
%    1000, power(10, -10))
% 
% [x,k] = sorMethod([],1,[],3,1,0,100,power(10,-10));
% 

mainDiagonal = [20,30,1,20];
subDiagonal = [8,3,1];
supraDiagonal = [8,3,1];
b = [8,12,34,6];
initialGuess =  [0,0,0,0];
[y,k] = sorMethod(subDiagonal, mainDiagonal, supraDiagonal, b, 0.5 , initialGuess, ...
    1000, power (10, -10));
 dependenceOfTheNumberOfIterationsOnOmega(subDiagonal,mainDiagonal, supraDiagonal, b, -10,10, ...
    1000, initialGuess, 10000, power(10,-10))

mainDiagonal = [20,30,1,20, 14];
subDiagonal = [8,3,1,2];
supraDiagonal = [8,3,1, 2];
b = [8,12,34,6, 10];
initialGuess =  [0,0,0,0, 0];
[y,k] = sorMethod(subDiagonal, mainDiagonal, supraDiagonal, b, 0.5 , initialGuess, ...
    1000, power (10, -20));
 dependenceOfTheNumberOfIterationsOnOmega(subDiagonal,mainDiagonal, supraDiagonal, b, -10,10, ...
    1000, initialGuess, 10000, power(10,-10))
