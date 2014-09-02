/*******************************************************************
  Camera setup 
 *******************************************************************/
import java.util.*;
import javax.media.opengl.*;
import com.sun.opengl.util.GLUT;
import javax.media.opengl.glu.GLU;


public class Camera 
{
	private enum ProjectionType { ORTHOGRAPHIC, PERSPECTIVE }

	private int id;
	public double xRes, yRes;
	public int viewportWidth, viewportHeight;
	private double cop[], look[], up[];
	private float u[], v[], n[];
	private double focalLength;
	private double nearClipDistance, farClipDistance;
	private ProjectionType projection;

	public Camera()
	{
		cop  = new double[3];
		look = new double[3];
		up   = new double[3];
	}

	public void setResolution( StringTokenizer stok )
	{
		// Reads resolution values from input string. Called from
		// render.cpp:readModelFile().

		xRes = Double.parseDouble( stok.nextToken() );
		yRes = Double.parseDouble( stok.nextToken() );

	}

	public void setCamera( int w, int h, GL gl )
	{
		GLU glu = new GLU();
		double fovangle, fovw, fovh;

		// Set the viewport width and height
		gl.glViewport( 0, 0, viewportWidth, viewportHeight );
		gl.glMatrixMode(GL.GL_PROJECTION);
		gl.glLoadIdentity();

		if( projection == ProjectionType.PERSPECTIVE ){

			// Compute the width and height of the nearPlane of the viewing volume
			// (Redbook page 126) 
			fovw = xRes * viewportWidth;
			fovh = yRes * viewportHeight;

			// Compute the field of view angle (Redbook page 126)
			fovangle = 2.0 * Math.atan2( fovh /2.0, focalLength ) * 180 / 3.1415928;

			// Setup the perspective camera (Redbook page 126)
			glu.gluPerspective(fovangle, fovw / fovh, 
					nearClipDistance, farClipDistance);

		} else {

			// Compute the width and height of the nearPlane of the viewing volume
			// (Redbook page 127) 
			fovw = 0.5 * xRes * viewportWidth;
			fovh = 0.5 * yRes * viewportHeight;

			// Setup the orthographic camera (Redbook page 127-8)
			gl.glOrtho(-fovw,fovw, -fovh, fovh, nearClipDistance, farClipDistance);

		}

		gl.glMatrixMode( GL.GL_MODELVIEW );
		gl.glLoadIdentity();

		// Position and point the camera in the right direction (Redbook page
		// 119-21)
		glu.gluLookAt( cop[0],  cop[1],  cop[2], 
				look[0], look[1], look[2], 
				up[0],   up[1],   up[2]);



		// System.out.println( "Camera.setCamera " );
	}

	public void setViewport( StringTokenizer stok )
	{
		viewportWidth  = Integer.parseInt( stok.nextToken() );
		viewportHeight = Integer.parseInt( stok.nextToken() );
	}

	public void setParameters( StringTokenizer stok )  
	{
		String pt = stok.nextToken();
		if (pt.equals( "perspective" ))
			projection = ProjectionType.PERSPECTIVE;
		else if (pt.equals( "orthographic" ))
			projection = ProjectionType.ORTHOGRAPHIC;

		cop[0] = Double.parseDouble ( stok.nextToken() );
		cop[1] = Double.parseDouble ( stok.nextToken() );
		cop[2] = Double.parseDouble ( stok.nextToken() );

		look[0] = Double.parseDouble ( stok.nextToken() );
		look[1] = Double.parseDouble ( stok.nextToken() );
		look[2] = Double.parseDouble ( stok.nextToken() );
		
		look=findVector(cop, look);

		up[0] = Double.parseDouble ( stok.nextToken() );
		up[1] = Double.parseDouble ( stok.nextToken() );
		up[2] = Double.parseDouble ( stok.nextToken() );

		focalLength      = Double.parseDouble ( stok.nextToken() );
		nearClipDistance = Double.parseDouble ( stok.nextToken() );
		farClipDistance  = Double.parseDouble ( stok.nextToken() );

	}

	public void print()
	{
		System.out.println( "--------- Camera ---------" );
		System.out.print( "ID :" + id );
		System.out.print( " xRes:" + xRes + " yRes:" + yRes );
		System.out.print( " viewport w:" + viewportWidth + " h:" + viewportHeight);
		System.out.println();
		System.out.print( " cop:" + cop[0] + " " + cop[1] + " " +  cop[2] );
		System.out.print( " look:" + look[0] + " " + look[1] + " " +  look[2] );
		System.out.print( " up:" + up[0] + " " + up[1] + " " +  up[2] );
		System.out.println();
		System.out.print( " u:" + u[0] + " " + u[1] + " " +  u[2] );
		System.out.println();
		System.out.print( " v:" + v[0] + " " + v[1] + " " +  v[2] );
		System.out.println();
		System.out.print( " n:" + n[0] + " " + n[1] + " " +  n[2] );
		System.out.println();
		System.out.print( " focal Length:" + focalLength );
		System.out.print( " near far Clip distance:" + nearClipDistance 
				+ " " + farClipDistance);
		System.out.print( " projection type:");
		switch ( projection )
		{
		case ORTHOGRAPHIC : System.out.print( "Orthographic" ); break;
		case PERSPECTIVE  : System.out.print( "Perspective" ); break;
		}
		System.out.println();
	}

	public double[] getCop()
	{
		return cop;
	}
	
	public boolean isOrthographic()
	{
		return this.projection==ProjectionType.ORTHOGRAPHIC;
	}
	
	public void printBasis()
	{
		float[][] basis = defineBasis();
		
		for (int i=0;i<3;i++)
		{
			for (int j=0;j<3;j++)
				System.out.print(basis[i][j]);
			System.out.println();
		}
	}

	public float getFocalLength()
	{
		return (float) this.focalLength;
	}
	
	public double[] findVector(double[] p1, double[] p2)
	{
		double[] result = {p2[0]-p1[0], p2[1]-p1[1], p2[2]-p1[2]};
		return result;
	}
	public float[][] defineBasis()
	{	
		this.n=Normalize(look());
		this.u=Normalize(computeXproduct(n, up()));
		this.v=Normalize(computeXproduct(n, u));
		
		float[][] result = {u, v, n};
		
		return result;

	}

	public float[] up()
	{
		float[] UP = new float[4];
		UP[0]=(float)up[0];
		UP[1]=(float)up[1];
		UP[2]=(float)up[2];

		return UP;
	}

	public double[] look()
	{
		double[] LOOK = new double[4];
		LOOK[0]=look[0];
		LOOK[1]=look[1];
		LOOK[2]=look[2];

		return LOOK;
	}

	public float[] computeXproduct (float[] v1, float[] v2)
	{
		float x=(v1[1]*v2[2]-v1[2]*v2[1]);
		float y=-(v1[0]*v2[2]-v1[2]*v2[0]);
		float z=(v1[0]*v2[1]-v1[1]*v2[0]);

		float[] result = {x, y, z, 0};

		return result;
	}

	public float[] Normalize(double[] u)
	{
		double magnitude=0; 
		float[] result=new float[4];

		for (int i=0;i<3;i++)
		{
			magnitude+=u[i]*u[i];
		}

		magnitude=Math.sqrt(magnitude);	

		for (int i=0;i<3;i++)
		{
			if (magnitude!=0)
				result[i]=(float) (u[i]/magnitude);
		}

		return result;
	}
	
	public float[] Normalize(float[] u)
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

		return u;
	}




}


