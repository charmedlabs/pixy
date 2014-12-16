% function LUT1=script6(drc, pfile, delta, tol, scale, miny, bits)

addpath(pwd);
cd('..\data');
close;

scale =16;
delta = 5;
tol = .9;
bits = 8;
miny = .05;

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
xx0 = x0;
xx1 = x1;
yy0 = y0;
yy1 = y1;

yc = (y1+y0)/2;
xc = (x1+x0)/2;

cx = (x0+x1)/2;
x0 = cx + (x0-cx)*scale;
x1 = cx + (x1-cx)*scale;
cy = (y0+y1)/2;
y0 = cy + (y0-cy)*scale;
y1 = cy + (y1-cy)*scale;

sc = 0;
if sign(xx0)~=sign(x0)
	sc = sc + 1;
end
if sign(xx1)~=sign(x1)
	sc = sc + 1;
end
if sign(yy0)~=sign(y0)
	sc = sc + 1;
end
if sign(yy1)~=sign(y1)
	sc = sc + 1;
end
if sc > 1
	'**** overflow'
end

[x0 x1 y0 y1]

S = [y0/x0 y0 x0 
	y0/x1 y0 x1
	y1/x0 y1 x0
	y1/x1 y1 x1];

[s0 rmin] = min(S(:,1));
[s1 rmax] = max(S(:,1));

if (sign(x0)~=sign(x1))
	'special case'
	S(rmin, 1) = 0;
	S(rmax, 1) = 0;
	[s0 rmin] = min(S(:,1));
	[s1 rmax] = max(S(:,1));
end

S(rmin, 1);
S(rmax, 1);

amin = atan2(S(rmin, 2), S(rmin, 3))*180/pi;
amax = atan2(S(rmax, 2), S(rmax, 3))*180/pi;
acen = atan2(yc, xc)*180/pi;
ycc = (S(rmin, 2) + S(rmax, 2))/2;
xcc = (S(rmin, 3) + S(rmax, 3))/2;
acen2 = atan2(ycc, xcc)*180/pi;

a1 = abs(amin-acen);
if a1>180
	a1 = 360 - a1;
end
a2 = abs(amax-acen);
if a2>180
	a2 = 360 - a2;
end

angle = a1+a2;




LUT = generatelut3(x0, x1, y0, y1, miny, bits);


plotlut3(LUT, bits, 'b');


cd('..\colormodel');