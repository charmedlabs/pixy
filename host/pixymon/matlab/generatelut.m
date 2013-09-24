function LUT=generatelut(L)

%C1 = [0:63, -64:-1];
C2 = [0:127, -128:-1];
LUT = [];
% bgr
for i=1:256
	for j=1:256
		c1 = C2(i)/127;
		c2 = C2(j)/127;
		if checkbounds(L, [c1, c2]) %& l>.1 %& max([r, g, b])<31 
			LUT = [LUT; 1];
		else
			LUT = [LUT; 0];
		end
	end
end