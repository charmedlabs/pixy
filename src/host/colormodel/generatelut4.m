function LUT=generatelut4(x0, x1, y0, y1, miny, bits)

LUT = zeros(2^(2*bits), 1);

pbits = bits;

for r=0:2^pbits-1
	r
	for g=0:2^pbits-1
		for b=0:2^pbits-1
			rp = r * 2^(8-pbits);
			gp = g * 2^(8-pbits);
			bp = b * 2^(8-pbits);
			y = (rp + gp + bp)/(3*255);
			P = [rp gp bp];
			C = map2(P);
			if (x0 <= C(1)) & (C(1) <= x1) & (y0 <= C(2)) & (C(2) < y1) & (y>miny)
				v = bitand(typecast(bitshift(typecast((int16(rp) - int16(gp)), 'uint16'), bits-9), 'uint8'), 2^bits-1);
				c1 = v(1);
				v = bitand(typecast(bitshift(typecast((int16(bp) - int16(gp)), 'uint16'), bits-9), 'uint8'), 2^bits-1);
				c2 = v(1);
				
				bin = uint32(c1)*2^bits+uint32(c2)+1;
				
				LUT(bin) = 1;
			end
		end
	end
end