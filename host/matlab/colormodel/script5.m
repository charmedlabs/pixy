% function LUT1=script5(drc, pfile, delta, tol, scale, miny, bits)

addpath(pwd);
cd('..\data');

scale = 0;
delta = 5;
tol = .9;

file = newfile();
eval(file); 
C1 = map2(pixels);

meanx = mean(C1(:, 1));
meany = mean(C1(:, 2));
x0 = iterate(C1(:, 1), meanx, -delta, tol);
x0 = x0 + (x0-meanx)*scale;
x1 = iterate(C1(:, 1), meanx, delta, tol);
x1 = x1 + (x1-meanx)*scale;
y0 = iterate(C1(:, 2), meany, -delta, tol);
y0 = y0 + (y0-meany)*scale;
y1 = iterate(C1(:, 2), meany, delta, tol);
y1 = y1 + (y1-meany)*scale;

[x0 x1 y0 y1]

cd('..\colormodel');