function plotlut(LX, LY, LUT)
hold on;

plot(LX(1:2), LY(1:2), '-');
plot(LX(3:4), LY(3:4), '-');
plot(LX(5:6), LY(5:6), '-');
plot(LX(7:8), LY(7:8), '-');
axis equal;

k = 1;
X = [];
Y = [];

%C1 = [0:63, -64:-1];
C2 = [0:127, -128:-1];
% c1|c2
for i=1:256
	for j=1:256
		c1 = C2(i)/127;
		c2 = C2(j)/127;
		if LUT(k)
				X = [X; c1];
				Y = [Y; c2];
		end
		k = k + 1;
	end
end

size(unique([X, Y], 'rows'));
plot(X, Y, '.');

hold off;