function bin=rgb2bin(r, g, b)

lutsize=21;
shift = 8-lutsize/3;

if 0
	r = bitshift(uint32(r)+4, -shift);
	if r>31
		r = 31;
	end
	g = bitshift(uint32(g)+4, -shift);
	if g>31
		g = 31;
	end
	b = bitshift(uint32(b)+4, -shift);
	if b>31
		b = 31;
	end
else
	r = bitshift(uint32(r), -shift);
	g = bitshift(uint32(g), -shift);
	b = bitshift(uint32(b), -shift);
end

bin = bitshift(r, lutsize/3*2) + bitshift(g, lutsize/3) + bitshift(b, 0);
