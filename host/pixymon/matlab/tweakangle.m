function ao=tweakangle(ai)
	diff = pi/2 - abs(ai);
	delta = 2*pi/(360*10); % 1/10 degree
	if diff < abs(delta)
		if (diff>0)
			ao = ai + delta;
		else
			ao = ai - delta;
		end
	else
		ao = ai;
	end
	
