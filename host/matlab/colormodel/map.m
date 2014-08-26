function C=map(pixels)

[N, c] = size(pixels);

C1 = [];
C2 = [];
C3 = [];

for i = 1:N
	R = double(pixels(i, 1));
	G = double(pixels(i, 2));
	B = double(pixels(i, 3));
	L = (R+G+B)/(3*255);

	C1 = [C1; (R-G)/255/L];
	C2 = [C2; (B-G)/255/L];
end
	
C = [C1, C2];