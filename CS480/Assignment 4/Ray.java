
public class Ray {

	public int n;
	public float[] u, p0;
	final float EPSILON=.0001f;

	public Ray(double[] COP, double[] pixel)
	{
		float p0[]={(float) COP[0], (float) COP[1], (float) COP[2], 1};
		this.u=new float[4];
		for (int i=0;i<3;i++)
			this.u[i]=(float) (pixel[i]-COP[i]);
		u[3]=0;
		Normalize();
		this.n=0;
	}
	
	public Ray (Point p, float[] u)
	{
		this.p0=p.toArray();
		this.u=u;
		Normalize();
		this.n=0;
	}
	
	public Ray()
	{
		this.p0=new float[4];
		this.u=new float[4];
		this.n=0;
	}
	
	public void setRay(float[] p, float[] u)
	{
		this.p0=p;
		this.u=u;
	}
	
	public void setPo(float[] p)
	{
		this.p0=p;
	}
	
	public void setVector (float[] u)
	{
		this.u=u;
		Normalize();
		//System.out.println(u[0]+" "+u[1]+" "+u[2]+" "+u[3]);
	}
	
	public Point toPoint()
	{
		Point p = new Point(p0);
		return p;
	}

	public void print()
	{
		System.out.println("P0= "+" "+p0[0]+" "+p0[1]+" "+p0[2]+" "+p0[3]);
		System.out.println("U= "+" "+u[0]+" "+u[1]+" "+u[2]+" "+u[3]);
	}
	
	public void Normalize()
	{
		double magnitude=0; 

		for (int i=0;i<4;i++)
		{
			magnitude+=u[i]*u[i];
		}

		magnitude=Math.sqrt(magnitude);	

		for (int i=0;i<4;i++)
		{
			u[i]=(float) (u[i]/magnitude);
		}
	}

}
