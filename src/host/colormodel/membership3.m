function MI=membership3(LUT1, LUT2, I)


for x = 1:size(I, 1)
	for y = 1:size(I, 2)
		r = I(x, y, 1);
		g = I(x, y, 2);
		b = I(x, y, 3);
		v = typecast(bitshift(typecast((int16(r) - int16(g)), 'uint16'), -1), 'uint8');
		c1 = v(1);
		v = typecast(bitshift(typecast((int16(b) - int16(g)), 'uint16'), -1), 'uint8');
		c2 = v(1);
		
		cs1 = typecast(c1, 'int8');
		cs2 = typecast(c2, 'int8');
				
		bin = uint32(c1)*256+uint32(c2)+1;
		
		if LUT1(bin) 
			if 1
				cs1 = typecast(c1, 'int8');
				cs2 = typecast(c2, 'int8');
				v = double(r)+double(g)+double(b);
				v = v/(3*255);
				if abs(cs1)>abs(cs2)
					i = uint32(c1)+1;
				else
					i = uint32(c2)+1;
				end
				if (LUT2(i, 1)<v) & (v<LUT2(i, 2))
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