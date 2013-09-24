function yi=iterateline(P, LI, d, e)

yi = LI(2);

while 1
	if boundtest(P, [LI(1), yi], sign(d))/size(P, 1) >= e
		break;
	end
	yi = yi + d;
end