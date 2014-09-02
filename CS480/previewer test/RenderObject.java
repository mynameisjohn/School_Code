import javax.media.opengl.GL;

public abstract class RenderObject
{
	public int id, matId;  // The object's unique ID and its material ID.

	// Each class that derives object should implement render()
	public abstract void render( GL gl, MaterialCollection materials ); 

    public abstract void print();
}


