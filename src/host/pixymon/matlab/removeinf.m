function A=removeinf(B)

A = [];
for i=1:size(B, 1)
	if B(i)==Inf | B(i)==-Inf
		continue;
	else
		A = [A; B(i)];
	end
end