function LUT1=script4(drc, pfile, delta, tol, scale, miny, bits)

addpath(pwd);
cd(drc);

eval(pfile);
C1 = map(pixels);

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

if 0
LUT1 = zeros(2^16, 1);
%LUT2 = zeros(2^16, 2);

for r=0:255
	r
	for g=0:255
		for b=0:255
			P = [r g b];
			C = map(P);
			if (x0 <= C(1)) & (C(1) <= x1) & (y0 <= C(2)) & (C(2) < y1) % & (v>minv)
				v = typecast(bitshift(typecast((int16(r) - int16(g)), 'uint16'), -1), 'uint8');
				c1 = v(1);
				v = typecast(bitshift(typecast((int16(b) - int16(g)), 'uint16'), -1), 'uint8');
				c2 = v(1);
				
				bin = uint32(c1)*256+uint32(c2)+1;
				
				LUT1(bin) = 1;
			end
		end
	end
end
end

LUT2 = generatelut3(x0, x1, y0, y1, miny, bits);

size(LUT2)

if 0
close
plotlut2(LUT2, 'b');
plotlut2(LUT1, 'g');
pause;
end

LUT1 = LUT2;		
pixfilter4(x0, x1, y0, y1, LUT1, bits, 'image1.png');
pixfilter4(x0, x1, y0, y1, LUT1, bits, 'image2.png');
pixfilter4(x0, x1, y0, y1, LUT1, bits, 'image2.png');
pixfilter4(x0, x1, y0, y1, LUT1, bits, 'image3.png');
pixfilter4(x0, x1, y0, y1, LUT1, bits, 'image4.png');
pixfilter4(x0, x1, y0, y1, LUT1, bits, 'image5.png');
pixfilter4(x0, x1, y0, y1, LUT1, bits, 'image6.png');
pixfilter4(x0, x1, y0, y1, LUT1, bits, 'image7.png');
pixfilter4(x0, x1, y0, y1, LUT1, bits, 'image8.png');

cd '../../colormodel'