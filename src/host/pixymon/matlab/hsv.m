function [h, s, v] = hsv(rgb)

	r = rgb(1);
	g = rgb(2);
	b = rgb(3);
	
	mini = min(rgb);
	maxi = max(rgb);
	v = maxi;
	delta = maxi - mini;
	if maxi ~= 0 
		s = delta / maxi;	
	else 
		s = 0;
		h = -1;
		return;
	end
	
	if r==maxi
		h = ( g - b ) / delta; %between yellow & magenta
	else 
		if g==maxi 
			h = 2 + ( b - r ) / delta;	% between cyan & yellow
		else
			h = 4 + ( r - g ) / delta;	% between magenta & cyan
		end
	end
	
	h = h*60;				
	if h < 0
		h = h + 360;
	end

