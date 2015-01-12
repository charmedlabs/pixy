function z=iterate(V, mean, delta, tol)

n = length(V);
z = mean;

if delta>0
	while 1 
		m = sum(z>V);
		if m/n >= tol
			break;
		end
		z = z + delta;
	end
else
	while 1 
		m = sum(z<V);
		if m/n >= tol
			break;
		end
		z = z + delta;
	end
end
		