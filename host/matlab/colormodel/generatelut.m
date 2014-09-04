function [LUT1 LUT2]=generatelut(L, mean)

miny = .12;
lf = 2.0;
uf = .40;

%C1 = [0:63, -64:-1];
C2 = [0:127, -128:-1];
LUT1 = [];
LUT2 = zeros(256, 2);
% bgr
for i=1:256
	for j=1:256
		c1 = C2(i)/127;
		c2 = C2(j)/127;
		if checkbounds(L, [c1, c2]) %& l>.1 %& max([r, g, b])<31 
			LUT1 = [LUT1; 1];
			if abs(c1)>abs(c2)
				[min max bin] = generatelut2(C2(i), mean(1), miny, [lf uf]);
			else
				[min max bin] = generatelut2(C2(j), mean(2), miny, [lf uf]);
			end
			LUT2(bin, :) = [min max];
		else
			LUT1 = [LUT1; 0];
		end
	end
end