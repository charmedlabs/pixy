% function LUT1=script5(drc, pfile, delta, tol, scale, miny, bits)

addpath(pwd);
cd('..\data');
close;

scale = 2.5;
delta = 5;
tol = .9;
bits = 8;
miny = .05

file = newfile('pixels')
eval(file); 
C1 = map2(pixels);

meanx = mean(C1(:, 1));
meany = mean(C1(:, 2));
x0 = iterate(C1(:, 1), meanx, -delta, tol);
x1 = iterate(C1(:, 1), meanx, delta, tol);
y0 = iterate(C1(:, 2), meany, -delta, tol);
y1 = iterate(C1(:, 2), meany, delta, tol);

[x0 x1 y0 y1]

cx = (x0+x1)/2;
x0 = cx + (x0-cx)*scale;
x1 = cx + (x1-cx)*scale;
cy = (y0+y1)/2;
y0 = cy + (y0-cy)*scale;
y1 = cy + (y1-cy)*scale;


[x0 x1 y0 y1]

if 1
LUT1 = generatelut4(x0, x1, y0, y1, miny, bits);
end

LUT = generatelut3(x0, x1, y0, y1, miny, bits);

file = newfile('lut')
eval(file); 

plotlut3(lut, bits, 'g');

pause;

plotlut3(LUT, bits, 'b');

if 1
pause

plotlut3(LUT1, 6, 'r');
end

cd('..\colormodel');