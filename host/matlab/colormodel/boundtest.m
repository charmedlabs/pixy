function n=boundtest(P, L, dir)

n = 0;

for i=1:size(P, 1)
	y = P(i, 1)*L(1) +  L(2);
	if dir > 0
		if P(i, 2) < y
			n = n + 1;
		end
	else
		if P(i, 2) > y
			n = n + 1;
		end
	end
end
