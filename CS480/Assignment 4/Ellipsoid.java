import java.util.*;
import javax.media.opengl.GL;
import com.sun.opengl.util.GLUT;

public class Ellipsoid extends RenderObject
{
	private double x,y,z;	     // Center of the ellpsoid.
	private double rx,ry,rz;   // Radii of the ellipsoid.
	private GLUT glut;

	public Quadric Q;
	
	public float[][] translate=new float[4][4];

	private final float EPSILON = .000001f;

	public Ellipsoid( StringTokenizer stok, int id_ )
	{
		glut = new GLUT();

		id = id_;

		stok.nextToken(); // strip away the token "mat"
		stok.nextToken(); // strip away the token "ID"
		matId = Integer.parseInt( stok.nextToken() );

		x  = Double.parseDouble( stok.nextToken() );
		y  = Double.parseDouble( stok.nextToken() );
		z  = Double.parseDouble( stok.nextToken() );
		rx = Double.parseDouble( stok.nextToken() );
		ry = Double.parseDouble( stok.nextToken() );
		rz = Double.parseDouble( stok.nextToken() );
		
		this.Q=new Quadric();
		
		float cRx=(float) (1/(rx*rx));
		float cRy=(float) (1/(ry*ry));
		float cRz=(float) (1/(rz*rz));
		
		Q.Q[0][0]=cRx;
		Q.Q[1][1]=cRy;
		Q.Q[2][2]=cRz;
		Q.Q[0][3]=(float) (-x*cRx);Q.Q[3][0]=(float) (-x*cRx);
		Q.Q[1][3]=(float) (-y*cRy);Q.Q[3][1]=(float) (-y*cRy);
		Q.Q[2][3]=(float) (-z*cRz);Q.Q[3][2]=(float) (-z*cRz);
		Q.Q[3][3]=(float) (-1+x*x*cRx+y*y*cRy+z*z*cRz);
		
		translate[0][3]=(float)x;
		translate[1][3]=(float)y;
		translate[2][3]=(float)z;
		translate[3][3]=1;
	}

	public void render( GL gl, MaterialCollection materials )
	{
		materials.setMaterial( matId, gl);

		gl.glPushMatrix();
		gl.glTranslated(x,y,z);
		gl.glScaled(rx,ry,rz);
		glut.glutSolidSphere(1,40,40);
		gl.glPopMatrix();
	}



	public float[] returnNormal(Point p)
	{
		return Normalize(Q.returnNormal(p));
	}

	public float findIntersections (Ray ray)
	{
		float a, b, c, d, n;
		float[]C;

		C=Q.returnC(ray.u, ray.p0);
		a=C[0];
		b=C[1];
		c=C[2];
		d=b*b-4*a*c;


		//System.out.println("d= "+d);

		if (d<0)//No intersection
			return 4000;

		else if (d<EPSILON)
		{
			float t=((-b)/(2*a));
			if (t>EPSILON)
				return t;
		}

		else
		{
			float tPlus=(float) ((-b+Math.sqrt(d))/(2*a));
			float tMinus=(float) ((-b-Math.sqrt(d))/(2*a));
			if (tMinus>EPSILON)
				return tMinus;
		}

		return 4000;

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
	
	public double returnX()
	{
		return this.x;
	}
	
	public double returnY()
	{
		return this.y;
	}
	
	public double returnZ()
	{
		return this.z;
	}



	public void print()
	{
		System.out.print( "Ellipsoid ID:" + id + " mat ID:" + matId );
		System.out.print( " x:" + x + " y:" + y + " z:" + z);
		System.out.print( " rx:" + rx + " ry:" + ry + " rz:" + rz);
		System.out.println();
	}
}

