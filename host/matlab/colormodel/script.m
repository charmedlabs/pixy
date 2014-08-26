function script(drc, pfile, delta, tol, scale)

addpath(pwd);
cd(drc);

eval(pfile);
C1 = map(pixels);
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

pixfilter(x0, x1, y0, y1, 'image1.png');
pixfilter(x0, x1, y0, y1, 'image2.png');
pixfilter(x0, x1, y0, y1, 'image2.png');
pixfilter(x0, x1, y0, y1, 'image3.png');
pixfilter(x0, x1, y0, y1, 'image4.png');
pixfilter(x0, x1, y0, y1, 'image5.png');
pixfilter(x0, x1, y0, y1, 'image6.png');
pixfilter(x0, x1, y0, y1, 'image7.png');
pixfilter(x0, x1, y0, y1, 'image8.png');

cd '../../colormodel'

