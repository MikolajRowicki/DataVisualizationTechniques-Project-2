function [] = dependenceOfTheNumberOfIterationsOnOmega(subDiagonal, ...
    mainDiagonal,supraDiagonal,b, minOmega, maxOmega, ...
    numberOfOmegas, initialGuess, maxNumberOfIterations, epsilon)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

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

omega = linspace(minOmega,maxOmega,numberOfOmegas);
omega = omega(omega ~= 0);
[~,k] = sorMethod(subDiagonal,mainDiagonal,supraDiagonal,b,omega, ...
    initialGuess, maxNumberOfIterations, epsilon);
plot(omega, k, 'LineWidth', 2)
end