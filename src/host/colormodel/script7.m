%function LUT1=script7(pfile, delta, tol, scale, miny, bits)

scale = 2;
delta = .005;
tol = .9;
bits = 8;
miny = .05;

addpath(pwd);
cd('..\data');

file = newfile('pixels')
eval(file); 

C1 = map(pixels);

meanx = mean(C1(:, 1))
meany = mean(C1(:, 2))
x0 = iterate(C1(:, 1), meanx, -delta, tol);
x1 = iterate(C1(:, 1), meanx, delta, tol);
y0 = iterate(C1(:, 2), meany, -delta, tol);
y1 = iterate(C1(:, 2), meany, delta, tol);

cx = (x0+x1)/2;
x0 = cx + (x0-cx)*scale;
x1 = cx + (x1-cx)*scale;
cy = (y0+y1)/2;
y0 = cy + (y0-cy)*scale;
y1 = cy + (y1-cy)*scale;

[x0 x1 y0 y1]
if 1
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

if 1
close
plotlut2(LUT2, 'b');
plotlut2(LUT1, 'g');
end

cd '../colormodel'