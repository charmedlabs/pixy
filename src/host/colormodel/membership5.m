function MI=membership4(x0, x1, y0, y1, LUT1, I, bits)


for x = 1:size(I, 1)
	for y = 1:size(I, 2)
		r = I(x, y, 1);
		g = I(x, y, 2);
		b = I(x, y, 3);
		v = bitand(typecast(bitshift(typecast((int16(r) - int16(g)), 'uint16'), bits-9), 'uint8'), 2^bits-1);
		c1 = v(1);
		v = bitand(typecast(bitshift(typecast((int16(b) - int16(g)), 'uint16'), bits-9), 'uint8'), 2^bits-1);
		c2 = v(1);
		
		cs1 = typecast(c1, 'int8');
		cs2 = typecast(c2, 'int8');
				
		bin = uint32(c1)*(2^bits)+uint32(c2)+1;
		
		if LUT1(bin) 
			if 1
				r = double(r);
				g = double(g);
				b = double(b);
				C = map2([r g b]);
				if (x0<C(1)) & (C(1)<x1) & (y0<C(2)) & (C(2)<y1)
					MI(x, y) = 1;
				else
					MI(x, y) = 0;
				end
			else
				MI(x, y) = 1;
			end
		else
			MI(x, y) = 0;
		end
	end
end