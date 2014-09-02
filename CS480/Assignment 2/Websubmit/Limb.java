
public class Limb {
	
	final public float[] paw_angle;
	final public float[] lower_angle;
	final public float[] upper_angle;
	
	public float body_angle;

	
	public float[] p0 = new float[3];
	
	public Limb (float x0, float y0, float z0)
	{
		
		this.paw_angle = new float[3];
		this.lower_angle = new float[3];
		this.upper_angle = new float[3];
		
		this.p0[0]=x0;
		this.p0[1]=y0;
		this.p0[2]=z0;
		
		if (p0[2]>0)
			this.body_angle = -45;
		else
			this.body_angle = 45;
		
	}
	
	public void reInit()
	{
		for (int i=0;i<3;i++)
			this.upper_angle[i]=0;
		
		for (int i=0;i<3;i++)
			this.lower_angle[i]=0;
		
		for (int i=0;i<3;i++)
			this.paw_angle[i]=0;
		

		
		if (p0[2]>0)
			this.body_angle = -45;
		else
			this.body_angle = 45;
	}

}
