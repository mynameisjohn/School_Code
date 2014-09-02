
public class Point {

	float x, y, z;

	public Point(float x, float y, float z)
	{
		this.x=x;
		this.y=y;
		this.z=z;
	}

	public Point (float[] p)
	{
		this.x=p[0];
		this.y=p[1];
		this.z=p[2];
	}

	public Point (float[] u, float[] p, float t)
	{
		this.x=p[0]+u[0]*t;
		this.y=p[1]+u[1]*t;
		this.z=p[2]+u[2]*t;
	}

	public Point()
	{
		this.x=0;
		this.y=0;
		this.z=0;
	}

	public float[] toArray()
	{
		float[] point = new float[4];

		point[0]=x;
		point[1]=y;
		point[2]=z;
		point[3]=1;

		return point;
	}

	public void print()
	{
		System.out.println("P= ("+x+", "+y+", "+z);
	}

	public float dist(Point p0)
	{
		float[] distance = new float[3];
		float magnitude = 0;
		distance[0]=this.x-p0.x;
		distance[1]=this.y-p0.y;
		distance[2]=this.z-p0.z;
		
		for (int i=0;i<3;i++)
			magnitude+=distance[i]*distance[i];
		
		magnitude=(float) Math.sqrt(magnitude);
		return magnitude;
		
	}
}
