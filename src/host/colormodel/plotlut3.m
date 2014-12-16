function plotlut3(LUT, bits, color)
hold on;

axis equal;
axis([-1 1 -1 1]);

k = 1;
X = [];
Y = [];

C2 = [0:(2^(bits-1)-1), -(2^(bits-1)):-1];
% c1|c2
for i=1:(2^bits)
	for j=1:(2^bits)
		c1 = C2(i)/(2^(bits-1)-1);
		c2 = C2(j)/(2^(bits-1)-1);
		if LUT(k)
				X = [X; c1];
				Y = [Y; c2];
		end
		k = k + 1;
	end
end

size(unique([X, Y], 'rows'));
plot(X, Y, ['.' color]);

h = plot(0, 0, 'r.');
hold off;