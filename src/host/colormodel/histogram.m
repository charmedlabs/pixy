function H=histogram(P)
H=zeros(2^15, 1);
for x = 1:size(P, 1)
	bin = bitshift(bitshift(uint16(P(x, 1)), -3), 10) + bitshift(bitshift(uint16(P(x, 2)), -3), 5) + bitshift(bitshift(uint16(P(x, 3)), -3), 0);
	H(bin) = H(bin) + 1;
end