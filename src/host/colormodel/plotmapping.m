U = zeros(1, 2^15);
V = U; 

for i=[1:2^15]
	[r g b]=bin2rgb(i);
	
	c = r + g + b;
	U(i) = (r-g)/c;
	V(i) = (b-(r+g)/2)/c;
end	

plot(U, V, 'o');
axis equal;	