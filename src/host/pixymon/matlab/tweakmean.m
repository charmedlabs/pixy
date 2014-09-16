function mo=tweakmean(mi)
min = .0001;

if abs(mi) < min
	if mi > 0
		mo = min;
	else
		mo = -min;
	end
else
	mo = mi;
end
	
