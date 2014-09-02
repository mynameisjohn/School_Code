import numpy as np

def relax(A,B,X,nit):
	for i in range (0,nit):
		for j in range (0,3):
			X[j]=0.
			X[j]=(B[j]-np.dot(A[j],X))/A[j,j]

x1=0.
x2=0.
x3=0.
B=np.matrix([[-3.],[2.],[0.]])
A=np.matrix([[2.,-1.,0.],[-1.,2.,-1.],[0.,-1.,1.]])
X=np.matrix([[x1],[x2],[x3]])

relax(A,B,X,2)
		
print X