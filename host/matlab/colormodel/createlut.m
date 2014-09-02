function LUT=createlut(x0, x1, y0, y1)

minv = .1;
lutsize = 21;

LUT=zeros(2^lutsize, 1);

for i=1:2^lutsize
	[r g b] = bin2rgb(i);
	
	C = map([r g b]);
	
	v = double(r + g + b)/(3*255);
	if (x0 <= C(1)) & (C(1) <= x1) & (y0 <= C(2)) & (C(2) < y1) & (v>minv)	
		LUT(i) = 1;
	else
		LUT(i) = 0;
	end
end
