function plotlut(L, LUT)
hold on;

X = [-10; 10];
Y = [L* [X(1); 1], L*[X(2); 1]]; 
plot(X, Y(1, 1:2));
plot(X, Y(2, 1:2));
plot(X, Y(3, 1:2));
plot(X, Y(4, 1:2));
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
plot(X, Y, '.');

plot(0, 0, 'r.');
hold off;