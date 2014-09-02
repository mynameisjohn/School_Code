import java.util.*;

import javax.media.opengl.GL;
import com.sun.opengl.util.GLUT;

public class Box extends RenderObject
{
	private GLUT glut;
	private double x, y, z; // Center of box
	private double basis[][]; // Orthogonal unit vectors for the 3 axis of box
	private double len[]; // lengths of the box along directions corresponding 
	// to the basis vectors
	private double mat[]; // Coordinate transformation matrix for OpenGL

	private final float EPSILON = .00001f;

	private float[][] allign = new float[4][4];

	Quadric q1;
	Quadric q2;
	Quadric q3;
	Quadric q4;
	Quadric q5;
	Quadric q6;

	private int hitId;

	public ArrayList<Quadric> Quadrics;

	public Box( StringTokenizer stok, int id_ )
	{
		basis = new double[3][3];
		len   = new double[3];
		mat   = new double[16];

		id = id_;

		stok.nextToken(); // strip away the token "mat"
		stok.nextToken(); // strip away the token "ID"
		matId = Integer.parseInt( stok.nextToken() );

		x  = Double.parseDouble( stok.nextToken() );
		y  = Double.parseDouble( stok.nextToken() );
		z  = Double.parseDouble( stok.nextToken() );

		for (int i=0; i<3; ++i)
			for (int j=0; j<3; ++j)
			{
				basis[i][j] = Double.parseDouble( stok.nextToken() );
				mat[4*i+j] = basis[i][j];
			}
		mat[15] = 1;

		for (int i=0; i<3; ++i)
			len[i] = Double.parseDouble( stok.nextToken() );

		this.Quadrics=new ArrayList<Quadric>();

		q1=new Quadric(1);
		q2=new Quadric(2);
		q3=new Quadric(3);
		q4=new Quadric(4);
		q5=new Quadric(5);
		q6=new Quadric(6);

		q1.Q[0][3]=.5f;q1.Q[3][0]=.5f;
		q1.Q[3][3]=(float) (-len[0]/2); 
		q2.Q[0][3]=-.5f;q2.Q[3][0]=-.5f;
		q2.Q[3][3]=(float) (-len[0]/2);

		q3.Q[1][3]=.5f;q3.Q[3][1]=.5f;
		q3.Q[3][3]=(float) (-len[1]/2);  
		q4.Q[1][3]=-.5f;q4.Q[3][1]=-.5f;
		q4.Q[3][3]=(float) (-len[1]/2);

		q5.Q[2][3]=.5f;q5.Q[3][2]=.5f;
		q5.Q[3][3]=(float) (-len[2]/2);  
		q6.Q[2][3]=-.5f;q6.Q[3][2]=-.5f;
		q6.Q[3][3]=(float) (-len[2]/2);

		Quadrics.add(q1);
		Quadrics.add(q2);
		Quadrics.add(q3);
		Quadrics.add(q4);
		Quadrics.add(q5);
		Quadrics.add(q6);

		q1.print();



		//I am not 100% on this transformation matrix
		for (int i=0;i<3;i++)
		{
			allign[0][i]=(float) basis[0][i];
			allign[1][i]=(float) basis[1][i];
			allign[2][i]=(float) basis[2][i];
		}
		allign[0][3]=(float) (-x*allign[0][0]-y*allign[0][1]-z*allign[0][2]);
		allign[1][3]=(float) (-x*allign[1][0]-y*allign[1][1]-z*allign[1][2]);
		allign[2][3]=(float) (-x*allign[2][0]-y*allign[2][1]-z*allign[2][2]);
		allign[3][3]=1;

		for (int i=0;i<4;i++)
		{
			for (int j=0;j<4;j++)
			{
				System.out.print(allign[i][j]+" ");
			}
			System.out.println();
		}

		System.out.println();

		for (int i=0;i<3;i++)
		{
			for (int j=0;j<3;j++)
			{
				System.out.print(basis[i][j]+" ");
			}
			System.out.println();
		}


		allign();

		q1.print();

		print();
	}

	public void allign()
	{
		for (Quadric q : Quadrics)
		{
			q.allign(this.allign);
		}
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

	public void render( GL gl, MaterialCollection materials )
	{
		double alpha, beta;

		glut = new GLUT();

		gl.glPushMatrix();
		gl.glTranslated( x, y, z );
		gl.glMultMatrixd( mat, 0 );
		materials.setMaterial( matId, gl);
		gl.glScaled( len[0], len[1], len[2] );
		glut.glutSolidCube( 1 );
		gl.glPopMatrix();
	}

	public float findIntersections(Ray ray)
	{
		float a, b, c, d, t=4000, t0=0;
		float[]C;
		

		//These form a sort of bounding box, not really


		float X = (float) ((len[0]/2)*Math.abs(basis[0][0])+(len[1]/2)*Math.abs(basis[0][1])+(len[2]/2)*Math.abs(basis[0][2]));
		float Y = (float) ((len[0]/2)*Math.abs(basis[1][0])+(len[1]/2)*Math.abs(basis[1][1])+(len[2]/2)*Math.abs(basis[1][2]));
		float Z = (float) ((len[0]/2)*Math.abs(basis[2][0])+(len[1]/2)*Math.abs(basis[2][1])+(len[2]/2)*Math.abs(basis[2][2]));
		/*
		System.out.println("X= "+X);
		System.out.println("Y= "+Y);
		System.out.println("Z= "+Z);
		 */

		for (Quadric q:Quadrics)
		{
			boolean inside=true;
			C=q.returnC(ray.u, ray.p0);
			a=C[0];
			b=C[1];
			c=C[2];
			d=b*b-4*a*c;

			t0=-c/b;

			Point pI = new Point (ray.u, ray.p0, t0);

			for (Quadric q0:Quadrics)
			{
				if (q0.id!=q.id)
					if (!q0.isInside(pI))
					{
						inside=false;
					}
			}
			if (inside)
				if (t0<t)
				{
					t=t0;
					this.hitId=q.id;
				}
			
			
		


			/*
			if (Math.abs(pI.x-x)<=X+EPSILON&&Math.abs(pI.y-y)<=(len[1]/2)+EPSILON&&Math.abs(pI.z-z)<=Z+EPSILON)
			{
				if (t0<t)
				{
					t=t0;
					this.hitId=q.id;
					if (pI.x==len[0]/2)
						pI.print();
				}
			}*/

		}
		//System.out.println("made it");
		return t;

		//pI.print();
	}

	public float[] returnNormal(Point p)
	{
		float[] N = new float[4];

		N=Normalize(Quadrics.get(hitId-1).multiply(p.toArray()));

		return N;
	}

	public boolean isInside (Point pI, int id)
	{
		boolean inside = true;
		//System.out.println("we got here");
		for (Quadric q:Quadrics)
		{
			if (q.id!=id)
				if (q.isInside(pI)==false)
					inside=false;
		}

		return inside;
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

	public void print()
	{
		System.out.print( "Box ID:" + id + " mat ID:" + matId );
		System.out.print( " x:" + x + " y:" + y + " z:" + z );
		System.out.print( " basis (" );
		for (int i=0; i<3; ++i)
			for (int j=0; j<3; ++j)
				System.out.print( basis[i][j]+ " ");

		System.out.print( " len:" + len[0] + " " + len[1] + " " + len[2]);
		System.out.print( " mat:" );
		for (int i=0; i<16; ++i)
			System.out.print( mat[i] + " " );
		System.out.println();

	}
}

