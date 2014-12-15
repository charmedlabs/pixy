function LUT1=script8(drc, pfile, delta, tol, scale, miny, bits)

addpath(pwd);
cd(drc);

eval(pfile);
C1 = map2(pixels);

meanx = mean(C1(:, 1))
meany = mean(C1(:, 2))
x0 = iterate(C1(:, 1), meanx, -delta, tol);
x0 = x0 + (x0-meanx)*scale;
x1 = iterate(C1(:, 1), meanx, delta, tol);
x1 = x1 + (x1-meanx)*scale;
y0 = iterate(C1(:, 2), meany, -delta, tol);
y0 = y0 + (y0-meany)*scale;
y1 = iterate(C1(:, 2), meany, delta, tol);
y1 = y1 + (y1-meany)*scale;

[x0 x1 y0 y1]

LUT1 = generatelut4(x0, x1, y0, y1, miny, bits);
if 1
close;
plotlut3(LUT1, bits, 'b');
end

pixfilter5(x0, x1, y0, y1, LUT1, bits, 'image1.png');
pixfilter5(x0, x1, y0, y1, LUT1, bits, 'image2.png');
pixfilter5(x0, x1, y0, y1, LUT1, bits, 'image2.png');
pixfilter5(x0, x1, y0, y1, LUT1, bits, 'image3.png');
pixfilter5(x0, x1, y0, y1, LUT1, bits, 'image4.png');
pixfilter5(x0, x1, y0, y1, LUT1, bits, 'image5.png');
pixfilter5(x0, x1, y0, y1, LUT1, bits, 'image6.png');
pixfilter5(x0, x1, y0, y1, LUT1, bits, 'image7.png');
pixfilter5(x0, x1, y0, y1, LUT1, bits, 'image8.png');

cd '../../colormodel'