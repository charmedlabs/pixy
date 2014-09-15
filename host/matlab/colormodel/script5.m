% function LUT1=script5(drc, pfile, delta, tol, scale, miny, bits)

addpath(pwd);
cd('..\data');
close;

scale = 2;
delta = 5;
tol = .9;
bits = 6;
miny = .05

file = newfile('pixels')
eval(file); 
C1 = map2(pixels);

meanx = mean(C1(:, 1));
meany = mean(C1(:, 2));
x0 = iterate(C1(:, 1), meanx, -delta, tol);
x1 = iterate(C1(:, 1), meanx, delta, tol);
c = (x0+x1)/2;
x0 = c + (x0-c)*scale;
x1 = c + (x1-c)*scale;
y0 = iterate(C1(:, 2), meany, -delta, tol);
y1 = iterate(C1(:, 2), meany, delta, tol);
c = (y0+y1)/2;
y0 = c + (y0-c)*scale;
y1 = c + (y1-c)*scale;


[x0 x1 y0 y1]


LUT = generatelut3(x0, x1, y0, y1, miny, bits);

file = newfile('lut')
eval(file); 

plotlut3(lut, 6, 'g');

pause;

plotlut3(LUT, 6, 'b');


cd('..\colormodel');