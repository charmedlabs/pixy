function plotlut(Pixels, L, LUT)
clf;
hold on;

X = [-200; 200];
Y = [L* [X(1); 1], L*[X(2); 1]]; 
plot(X, Y(1, 1:2));
plot(X, Y(2, 1:2));
plot(X, Y(3, 1:2));
plot(X, Y(4, 1:2));
axis equal;
axis([-128 128 -128 128]);

k = 1;
X = [];
Y = [];

Map = [0:127, -128:-1];

for i=Map
	for j=Map
		if LUT(k)
			X = [X; i];
			Y = [Y; j];
		end
		k = k + 1;
	end
end


plot(X, Y, 'b.');


plot(Pixels(1:end, 1), Pixels(1:end, 2), 'g.');
plot(0, 0, 'r.');
hold off;