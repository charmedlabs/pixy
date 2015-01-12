function [max, bin]=maxbin(H)
max = -1;
for i=1:2^15
	if H(i)>max
		max = H(i);
		bin = i;
	end
end
	