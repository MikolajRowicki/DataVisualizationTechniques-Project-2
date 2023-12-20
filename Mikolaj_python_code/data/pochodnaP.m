function [y] = pochodnaP(x,h,f)
%POCHODNAP oblicza przybliżoną wartość pochodnej funkcji f
X = repmat(x',1,length(h));
H = repmat(h, length(x), 1);
tabela_1 = f(X-H);
tabela_2 = f(X+H);
y = (tabela_2 - tabela_1) ./ (2 * H);
end