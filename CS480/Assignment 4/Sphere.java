import java.util.*;
import javax.media.opengl.GL;
import com.sun.opengl.util.GLUT;

public class Sphere extends RenderObject
{
	private double x, y, z, radius; // center and radius of sphere
	private GLUT glut;

	public Quadric q;

	private final float EPSILON = .0001f;

	public Sphere( StringTokenizer stok, int id_ )
	{
		glut = new GLUT();

		id = id_;

		stok.nextToken(); // strip away the token "mat"
		stok.nextToken(); // strip away the token "ID"
		matId = Integer.parseInt( stok.nextToken() );

		x  = Double.parseDouble( stok.nextToken() );
		y  = Double.parseDouble( stok.nextToken() );
		z  = Double.parseDouble( stok.nextToken() );
		radius  = Double.parseDouble( stok.nextToken() );



		q=new Quadric();
		for (int i=0;i<3;i++){
			q.Q[i][i]=1;
			//System.out.println(q.Q[i][i]);
		}
		q.Q[3][3]=(float) (-radius*radius+x*x+y*y+z*z);
		q.Q[3][0]=(float)-x;q.Q[0][3]=(float)-x;
		q.Q[3][1]=(float)-y;q.Q[1][3]=(float)-y;
		q.Q[3][2]=(float)-z;q.Q[2][3]=(float)-z;

		//q.print();


	}

	public void render( GL gl, MaterialCollection materials )
	{
		gl.glPushMatrix();
		gl.glTranslated(x,y,z);
		materials.setMaterial(matId, gl);
		glut.glutSolidSphere(radius,40,40);
		gl.glPopMatrix();
	}

	public void print()
	{
		System.out.print( "Sphere ID:" + id + " mat ID:" + matId );
		System.out.print( " x:" + x + " y:" + y + " z:" + z);
		System.out.print( " radius:" + radius );
		System.out.println();

	}

	public float findIntersections (Ray ray)
	{
		float a, b, c, d, n;
		float[]C;

		C=q.returnC(ray.u, ray.p0);
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




	public float[] returnNormal(Point p)
	{
		return Normalize(q.returnNormal(p));
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
	
	public boolean isInside (Point pI)
	{
		return true;
	}




}

