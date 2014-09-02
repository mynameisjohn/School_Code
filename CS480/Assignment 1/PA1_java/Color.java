package P1;
/**
 * RGB Color 
 * 
 * @author Stan Sclaroff <sclaroff>
 */

public class Color {
	
	private float r,g,b;
		
	public Color()
	{
		//default initial color is white
		this.r=(float)1.0; 
		this.g=(float)1.0;
		this.b=(float)1.0;
	}
	
	public Color(float r, float g, float b)
	{
		this.setColor(r,g,b);
	}
	
	// set color 
	public void setColor(float r, float g, float b)
	{
		// r, g, b values must be in range 0:1
		this.r = clampColorComponent(r);
		this.g = clampColorComponent(g);
		this.b = clampColorComponent(b);
	}
	
	// get red, green, and blue values
	public float getRed()
	{
		return this.r;
	}
	
	public float getGreen()
	{
		return this.g;	
	}	
	
	public float getBlue()
	{
		return this.b;
	}
	
	// private helper to clamp value within range 0,1]
	private float clampColorComponent(float v)
	{
		if(v > 1.0)
			v=(float)1.0;
		else if(v < 0.0)
			v=(float)0.0;
		return v;
	}
}
