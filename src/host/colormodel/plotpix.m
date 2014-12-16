function C=plotpix(pixels)

[N, c] = size(pixels);

C1 = [];
C2 = [];
C3 = [];

for i = 1:N
	R = pixels(i, 1);
	G = pixels(i, 2);
	B = pixels(i, 3);
	L = (R+G+B)/(3*255);
	if 1 %(L>.10) & (max([R, G, B])<255)
		C1 = [C1; L];
		C2 = [C2; (R-G)/255];
		C3 = [C3; (B-G)/255];
	end
end
	
C = [C1, C2, C3];

