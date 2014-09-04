function plotlut2(LUT, color)
hold on;

axis equal;
axis([-1 1 -1 1]);

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
plot(X, Y, ['.' color]);

h = plot(0, 0, 'r.');
hold off;