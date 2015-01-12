function LUT1=generatelut2(x0, x1, y0, y1, miny, bits)

C2 = [0:2^(bits-1)-1, -2^(bits-1):-1];
LUT1 = [];

S = [y0/x0 y0 x0 
	y0/x1 y0 x1
	y1/x0 y1 x0
	y1/x1 y1 x1];
S(:, 1);

[s0 rmin] = min(S(:,1));
[s1 rmax] = max(S(:,1));

%if (sign(y0)==sign(y1)) & (sign(x0)~=sign(x1))
if sign(x0)~=sign(x1)
	'special case'
	S(rmin, 1) = 0;
	S(rmax, 1) = 0;
	[s0 rmin] = min(S(:,1));
	[s1 rmax] = max(S(:,1));
end


for i=1:2^bits
	for j=1:2^bits
		c1 = C2(i)/(2^(bits-1)-1);
		c2 = C2(j)/(2^(bits-1)-1);
		
		if sqrt(c1^2+c2^2)<miny
			LUT1 = [LUT1; 0];
		else	
			if sign(x0)==sign(x1)
				if (sign(c1)==sign(x0)) & (s0 < c2/c1) & (c2/c1 < s1) 
					LUT1 = [LUT1; 1];
				else
					LUT1 = [LUT1; 0];
				end
			else
				if sign(c2)==sign(y0) 
					if 0 %sign(x0)==sign(x1)
						if (s0 < c2/c1) & (c2/c1 < s1)
							LUT1 = [LUT1; 1];
						else
							LUT1 = [LUT1; 0];
						end
					else
						if (s1 < c2/c1) | (c2/c1 < s0)
							LUT1 = [LUT1; 1];
						else
							LUT1 = [LUT1; 0];
						end					
					end
				else
					LUT1 = [LUT1; 0];
				end		
			end
		end
	end
end

