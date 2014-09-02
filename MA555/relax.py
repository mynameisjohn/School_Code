# Relax - solves the system of equations
#    Ax=b
# using an iterative Gauss-Seidel relaxation algorithm

import numpy as np

#takes in A, b, and the number of relaxation iterations
def relax(A,b,nit):
	dim=len(A)
	#initialize x (our initial guess) to zero
	x=np.zeros([dim,1])
	for i in np.arange (0,nit):
		for j in np.arange (0,dim):
			x[j]=0.
			x[j]=(b[j]-np.dot(A[j],x)) / A[j,j]
	return x
	
# [ 2 -1  0] [x1]   [-3 ]
# [-1  2 -1] [x2] = [ 2 ]
# [ 0 -1  1] [x3]   [ 0 ]
#A test case which solves the above system and stores the result in x
nit = 10
b = np.matrix([[-3.],[2.],[0.]])
A = np.matrix([[2.,-1.,0.],[-1.,2.,-1.],[0.,-1.,1.]])
x = relax(A,b,nit)

#the correct answer is [-1, 1, 1]
print x