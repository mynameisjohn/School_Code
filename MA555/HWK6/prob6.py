import numpy as np

def f(X):
	return X[0,0]**2-np.cos(X[1,0])

def g(X):
	return np.sin(X[0,0])+X[0]**2+X[1,0]**3

def jacob(X):
	J=np.matrix([[2*X[0,0],np.sin(X[1,0])],[np.cos(X[0,0])+2*X[0,0],3*X[1,0]**2]])
	return J

def relax(A,B,X,nit):
	for i in range (0,nit):
		for j in range (0,2):
			X[j]=0.
			X[j]=(B[j]-np.dot(A[j],X))/A[j,j]

X=np.matrix([[1.5],[-1.25]])

F=np.matrix([[f(X)],[g(X)]])

dX=np.matrix('0.;0.')

for i in range (0,1):
	J=jacob(X)
	relax(J,F,dX,10)
	X=X-dX
	F[0,0]=f(X)
	F[1,0]=g(X)
	print "X:"
	print X
	print "dX:"
	print dX
	print " "

print F