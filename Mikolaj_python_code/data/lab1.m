a=5;
a = 5;

s = 'Matlab';
p = 'Octave';
double(s)
char(ans)
napis = ['język', s]
pion = [s;p]
length(s)
strcmp(s,p)
strncmp(s,p,3)
upper(s)
lower(s)
C = {1,2,3;
    'text',rand(5,10,2),{11;22;33}}
strukturka.a = 1
int2str(8)
num2str([2,3,4])
str2double('2.7')
p(2)
n = 1/3
% format short  /format long zmienna %
1e3
10E3
2i+5
2^3
x = 10;
exp(x)
log(x)
log10(x)
log2(x)
round(exp(x))
floor(x)
ceil(x)
fix(x)
A = [2,3,4;5,6,7]
B = [1 2; 11 8; 7 3]
C = 4:6
D = [2;3]
E = [2 -1; -2 0] + i*[3 1.5; 2 3]

X = [A' B]
X = [A' B;[1:4]]
X(2,3)
X(9)
X(:,2:3)
X(1:2,:)
X(:,[1,2])
X(:,2:3) = []
A*B


% Transponujemy macierz apostrofem '

% 1.2a 
x = 4 * 10 ^ 2;

% 1.2 b
w2 = [-2:0.2:2];
w1 = w2';

% 1.2 c
A = [-4, 6, 0; 1, -2, 6; 5, 9, 1];
f = [3; 0; 8];
size(A)
size(f)
size_A = size(A);
size_f = size(f);

%1.2 d
A = rand(4,4);
B = rand(4,4);
wyr1 = (A + B)*(A + B) + 2*(A-B);
save lab1.mat A B wyr1 
load lab1.mat A B wyr1

%1.2 e
A = rand(4,4);
B = rand(4,4);
C = (A + B')/2;

%1.2 f
A = rand(3,3);
B = rand(3,3);
A.^2
A*B

%1.2 g
C = [4,1;7,2];
x = [1,4];
% C*x - nie zadziała (nieprawidłowe wymiary)
C.*x

%1.2 h
A = rand(3,5);
B = rand(5,4);
C1 = zeros(3,4);
C2 = zeros(3,4);

tic
C1 = A*B
toc

tic
for i = 1:3
        for j = 1:4
            for k = 1:5
                C2(i, j) = C2(i, j) + A(i, k) * B(k, j);
            end
        end
end
toc

%1.2 i
A = rand(4,4);
detA = det(A)
eigenValuesA = eig(A)
coefficientA = poly(A)
rankA = rank(A)

%1.2 j
w = rand(1,12);
Y = reshape(w, 3,4);

%1.2k
a = 7/3
b = 2.33
a^0
b^0
a^1
b^1
a^2
b^2
a^3
b^3
a^4
b^4
a^5
b^5
a^6
b^6
a^7
b^7
abs(a^0-b^0)
abs(a^1-b^1)
abs(a^2-b^2)
abs(a^3-b^3)
abs(a^4-b^4)
abs(a^5-b^5)
abs(a^6-b^6)
abs(a^7-b^7)

%1.2 l
wyr1 = (sin(pi/4))^5
wyr2 = log(sqrt(5))
wyr3 = abs(3-2*i)

%1.2 m
x = [0,pi/2,pi,3*pi/2,2*pi]
y = myfunc(x)

%1.2 n
wynik = heron(3,4,5)
function P = heron(a,b,c)
p = (a+b+c)/2
P = sqrt(p*(p-a)*(p-b)*(p-c))
end

