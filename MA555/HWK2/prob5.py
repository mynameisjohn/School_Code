import math

def prob5():
	x=7.5
	y=1.0
	n=0
	while (math.exp(x)-y) > 1e-7:
		n=n+1
		y=y+(1.0/math.factorial(n))*x**n
	print n