function so=tweakslope(si)
	if si > 1000
		so = 1000;
	else 
		if si < -1000
			so = -1000;
		else
			so = si;
		end
	end
	
