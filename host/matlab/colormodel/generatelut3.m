function LUT1=generatelut2(x0, x1, y0, y1)

miny = .05;

C2 = [0:127, -128:-1];
LUT1 = [];


S = [y0/x0 y0/x1 y1/x0 y1/x1];
s0 = min(S);
s1 = max(S);

for i=1:256
	for j=1:256
		c1 = C2(i)/127;
		c2 = C2(j)/127;
		
		if sqrt(c1^2+c2^2)<miny
			LUT1 = [LUT1; 0];
		else	
		
			if sign(x0)==sign(x1)
				if (s0 < c2/c1) & (c2/c1 < s1) & (sign(c1)==sign(x0))
					LUT1 = [LUT1; 1];
				else
					LUT1 = [LUT1; 0];
				end
			else
				if (s0 < c2/c1) & (c2/c1 < s1) & (sign(c2)==sign(y0))
					LUT1 = [LUT1; 1];
				else
					LUT1 = [LUT1; 0];
				end		
			end
		end
	end
end

