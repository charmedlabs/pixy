function [r g b]=bin2rgb(bin)

lutsize=21;
mask = 2^(lutsize/3) - 1;
shift = 8-lutsize/3;

b = bitand(bitshift(bin, 0), mask);
g = bitand(bitshift(bin, -lutsize/3), mask);
r = bitand(bitshift(bin, -lutsize/3*2), mask);

r = bitshift(r, shift);
g = bitshift(g, shift);
b = bitshift(b, shift);
	