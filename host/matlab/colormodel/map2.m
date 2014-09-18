function C=map2(pixels)

m = 32768; 

[N, c] = size(pixels);

C1 = [];
C2 = [];
C3 = [];

for i = 1:N
	R = double(pixels(i, 1));
	G = double(pixels(i, 2));
	B = double(pixels(i, 3));
	L = R+G+B;

	C1 = [C1; fix((R-G)*m/L)];
	%C2 = [C2; (B-(G+R)/2)/L];
	C2 = [C2; fix((B-G)*m/L)];
end
	
C = [C1, C2];