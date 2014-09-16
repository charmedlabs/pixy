H = [];

for i = 1:size(R, 2)
	h = hsv([R(1, i, 1), R(1, i, 2), R(1, i, 3)]);
	H = [H, h];
end