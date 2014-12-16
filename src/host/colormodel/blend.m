function I=blend(I, MI)

for x = 1:size(I, 1)
	for y = 1:size(I, 2)
		if MI(x, y)
			I(x, y, 1) = 255;
			I(x, y, 2) = 0;
			I(x, y, 3) = 0;
		end
	end
end