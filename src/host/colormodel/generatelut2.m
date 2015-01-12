function [min max bin]=generatelut2(c, mean, miny, factor)

			
	bin = uint16(typecast(int8(c), 'uint8'))+1;
	y = 2*c/mean; % mean = c1/y  --- 2 because we have the difference /2
			
	y = y/(3*255);
	min = y*1/(1+factor(1));
	max = y*(1+factor(2));
	
	if min<miny
		min = miny;
	end
	if max>1.0
		max = 1.0;
	end
	if max<min
		max = 0;
		min = 0;
	end

