
public class Quadric {

	public float[][] Q;

	private final float EPSILON = .00001f;

	int id;

	public Quadric()
	{
		this.Q=new float[4][4];
		this.id=0;
	}

	public Quadric(int id)
	{
		this.Q=new float[4][4];
		this.id=id;
	}

	public void multiply(float[][] M)
	{
		float[][] result = new float [4][4];

		for (int i=0;i<4;i++)
			for (int j=0;j<4;j++)
				for (int k=0;k<4;k++)
					result[i][j]+=Q[i][k]*M[k][j];

		Q=result;
	}

	public float[][] multiply(float[][] M1, float[][] M2)
	{
		float[][] result = new float [4][4];

		for (int i=0;i<4;i++)
			for (int j=0;j<4;j++)
				for (int k=0;k<4;k++)
					result[i][j]+=M1[i][k]*M2[k][j];

		return result;
	}


	public float[] multiply(float[] M)
	{
		float[] result = new float [4];

		for (int i=0;i<4;i++)
			for (int j=0;j<4;j++)
				result[i]+=M[i]*Q[i][j];

		return result;
	}

	public void allign (float[][] allign)
	{
		float[][] allignT = new float [4][4];

		for (int i=0;i<4;i++)
			for (int j=0;j<4;j++)
				allignT[i][j]=allign[j][i];
		/*
		System.out.println("Allign T");
		for (int i=0;i<4;i++)
		{
			for (int j=0;j<4;j++)
			{
				System.out.print(allign[i][j]+" ");
			}
			System.out.println();
		}*/

		this.Q = multiply(allignT, Q);
		//print();
		this.Q = multiply(Q, allign);
		//print();
	}

	public float[] returnNormal(Point p)
	{
		float[] pI=p.toArray();
		float[] N=new float[4];

		for (int i=0;i<3;i++)
			N[i]=(Q[0][i]*pI[0]+Q[1][i]*pI[1]+Q[2][i]*pI[2]+Q[3][i]*pI[3]);

		return N;
	}



	public float[] returnC(float[] u, float[] p0)
	{
		float a=0, b=0, c=0, d, e;
		float[] uTq=new float[4], pTq=new float[4];
		/*	
		for (int i=0;i<4;i++)
		{
			System.out.print(u[i]);
		}
		System.out.println();
		 */	
		for (int i=0;i<4;i++)
		{
			uTq[i]=(Q[0][i]*u[0]+Q[1][i]*u[1]+Q[2][i]*u[2]+Q[3][i]*u[3]);
			pTq[i]=(Q[0][i]*p0[0]+Q[1][i]*p0[1]+Q[2][i]*p0[2]+Q[3][i]*p0[3]);
		}


		//a=u[0]*uTq[0]+u[1]*uTq[1]+u[2]*uTq[2]+u[3]*uTq[3];

		for (int i=0;i<4;i++)
		{
			a+=u[i]*uTq[i];
			b+=p0[i]*(2*uTq[i]);
			c+=p0[i]*pTq[i];
		}
		/*
		System.out.println("a= "+a);
		System.out.println("b= "+b);
		System.out.println("c= "+c);
		 */

		float C[]={a, b, c};

		return C;
	}

	public void print()
	{
		System.out.println("Quadric "+this.id);
		for (int i=0;i<4;i++)
		{
			for (int j=0;j<4;j++)
			{
				System.out.print(Q[i][j]+" ");
			}
			System.out.println();
		}
	}

	public boolean isInside (Point pI)
	{
		float[] p = pI.toArray();
		float[] pTq = new float[4];
		float result=0;

		for (int i=0;i<4;i++)
			pTq[i]=(Q[0][i]*p[0]+Q[1][i]*p[1]+Q[2][i]*p[2]+Q[3][i]*p[3]);
		for (int j=0;j<4;j++)
			result+=p[j]*pTq[j];

		//System.out.println(result);

		if (result>0)
			return false;
		else
			return true;
	}
}
