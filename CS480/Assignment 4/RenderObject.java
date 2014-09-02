import javax.media.opengl.GL;

public abstract class RenderObject
{
	public int id, matId;  // The object's unique ID and its material ID.
	
	private final float EPSILON = .000001f;

	// Each class that derives object should implement render()
	public abstract void render( GL gl, MaterialCollection materials ); 

    public abstract void print();
    
    public abstract float findIntersections(Ray ray);
    
    public abstract float[] returnNormal(Point p);
    
    public abstract double returnX();
    
    public abstract double returnY();
    
    public abstract double returnZ();

    
}


