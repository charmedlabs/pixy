function MI=membership(x0, x1, y0, y1, I)

minv = .12;

for x = 1:size(I, 1)
	for y = 1:size(I, 2)
		P = [I(x, y, 1), I(x, y, 2), I(x, y, 3)];
		C = map(P);
		v = double(sum(P))/(3*255);
		if (x0 <= C(1)) & (C(1) <= x1) & (y0 <= C(2)) & (C(2) < y1) & (v>minv)
			MI(x, y) = 1;
		else
			MI(x, y) = 0;
		end
	end
end