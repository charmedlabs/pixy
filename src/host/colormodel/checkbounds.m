function b=checkbounds(L, P)

Y = L(1:end, 1)*P(1) + L(1:end, 2);

if (Y(1)>P(2)) & (Y(2)<P(2)) & (Y(5)>P(2)) & (Y(6)<P(2))
	b = 1;
else
	b = 0;
end