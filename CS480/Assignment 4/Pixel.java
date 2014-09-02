
import java.awt.Color;

public class Pixel {

	Point location;
	Color color;
	
	public Pixel(Point p, Color c)
	{
		this.location=p;
		this.color=c;
	}
	
	public Pixel()
	{
		this.location=new Point();
		this.color=new Color (0, 0, 0);
	}
	
	public Pixel (float x, float y, float z, float r, float g, float b)
	{
		this.location = new Point (x, y, z);
		this.color = new Color (r, g, b);
	}
	
	public Pixel (Point p, float r, float g, float b)
	{
		this.location = p;
		this.color = new Color (r, g, b);
	}
	
	
	
}
