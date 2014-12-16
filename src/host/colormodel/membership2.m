function MI=membership2(LUT, I)

for x = 1:size(I, 1)
	for y = 1:size(I, 2)
		bin = rgb2bin(I(x, y, 1), I(x, y, 2), I(x, y, 3)) + 1;
		
		if LUT(bin)
			MI(x, y) = 1;
		else
			MI(x, y) = 0;
		end
	end
end