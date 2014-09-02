import java.util.*;
import javax.media.opengl.GL;
import javax.media.opengl.glu.GLUquadric;
import javax.media.opengl.glu.GLU;


public class Cylinder extends RenderObject
{
	private double x, y, z; // Center of base of cylinder
	private double nx, ny, nz; // A (non necessarily unit) vector along axis of
	// cylinder
	private double radius, length; 

	public Quadric q1, q2, q3;

	public int hitId;

	private final float EPSILON = .0001f;

	public ArrayList<Quadric> Quadrics;

	float[][] basis;

	public Point pC;

	float[][] allign = new float[4][4];

	public Cylinder( StringTokenizer stok, int id_ )
	{

		id = id_;

		stok.nextToken(); // strip away the token "mat"
		stok.nextToken(); // strip away the token "ID"
		matId = Integer.parseInt( stok.nextToken() );

		x  = Double.parseDouble( stok.nextToken() );
		y  = Double.parseDouble( stok.nextToken() );
		z  = Double.parseDouble( stok.nextToken() );
		nx = Double.parseDouble( stok.nextToken() );
		ny = Double.parseDouble( stok.nextToken() );
		nz = Double.parseDouble( stok.nextToken() );
		radius = Double.parseDouble( stok.nextToken() );
		length = Double.parseDouble( stok.nextToken() );
		defineBasis();

		this.pC=new Point((float)x, (float)y, (float)z);

		this.Quadrics=new ArrayList<Quadric>();

		q1=new Quadric(1);
		q2=new Quadric(2);
		q3=new Quadric(3);

		q1.Q[3][2]=.5f;q1.Q[2][3]=.5f;
		q1.Q[3][3]=(float) (-length/2);

		float R = (float) (1/(radius*radius));
		q2.Q[0][0]=R;
		q2.Q[1][1]=R;
		q2.Q[3][3]=-1;
		/*
		q2.Q[0][0]=1;
		q2.Q[1][1]=1;
		q2.Q[3][0]=(float) -x;q2.Q[0][3]=(float) -x;
		q2.Q[3][1]=(float) -y;q2.Q[1][3]=(float) -y;
		q2.Q[3][3]=(float) (-radius*radius+x*x+y*y);*/

		q3.Q[3][2]=-.5f;q3.Q[2][3]=-.5f;
		q3.Q[3][3]=(float) (-length/2);

		//q1.print();
		q2.print();
		//q3.print();

		for (int i=0;i<3;i++)
		{
			allign[0][i]=basis[0][i];
			allign[1][i]=basis[1][i];
			allign[2][i]=basis[2][i];
		}

		allign[0][3]=(float) -x;
		allign[1][3]=(float) -y;
		allign[2][3]=(float) -z;
		allign[3][3]=1;

		Quadrics.add(q1);
		Quadrics.add(q2);
		Quadrics.add(q3);

		allign();
		//print();
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

	public float findIntersections (Ray ray)
	{
		float a, b, c, d, t=4000, t0=0;
		float[]C;
		boolean inside;

		for (Quadric q:Quadrics)
		{
			C=q.returnC(ray.u, ray.p0);
			a=C[0];
			b=C[1];
			c=C[2];
			d=b*b-4*a*c;

			if (q.id==2)
			{

				//System.out.println("d= "+d);
				if (d<0)//No intersection
					t0=4000;

				else if (d<EPSILON)
				{
					float tTan=((-b)/(2*a));
					if (tTan>EPSILON)
						t0=tTan;
				}

				else
				{
					float tPlus=(float) ((-b+Math.sqrt(d))/(2*a));
					float tMinus=(float) ((-b-Math.sqrt(d))/(2*a));
					if (tMinus>EPSILON)
						t0=tMinus;
				}

				if (t0<t)
				{
					Point pI = new Point (ray.u, ray.p0, t0);
					if (q1.isInside(pI)&&q3.isInside(pI))
					{
						t=t0;
						this.hitId=q.id;
					}
				}
			}
			else
			{
				t0=-c/b;

				Point pI = new Point (ray.u, ray.p0, t0);

				if (q2.isInside(pI))
					if (t0<t)
					{
						t=t0;
						this.hitId=q.id;
					}
			}
		}
		return t;
	}

	public void allign()
	{
		System.out.println("after");
		for (Quadric q : Quadrics)
		{
			q.allign(this.allign);
			//q.print();
		}
		//q2.print();
	}

	public void defineBasis()
	{
		float[] n = {(float) nx, (float) ny, (float) nz, 0};
		float[] u = {(float) nz, 0, (float) -nx, 0};
		u=Normalize(u);
		float[] v = Normalize(computeXproduct (n, u));

		this.basis = new float[][] {u, v, n, {0, 0, 0, 1}};

		System.out.print( " u:" + u[0] + " " + u[1] + " " +  u[2] );
		System.out.println();
		System.out.print( " v:" + v[0] + " " + v[1] + " " +  v[2] );
		System.out.println();
		System.out.print( " n:" + n[0] + " " + n[1] + " " +  n[2] );
		System.out.println();
	}

	public float[] returnNormal(Point p)
	{
		float[] N = new float[4];

		N=Normalize(Quadrics.get(hitId-1).multiply(p.toArray()));

		return N;
	}

	public float[] computeXproduct (float[] v1, float[] v2)
	{
		float x=(v1[1]*v2[2]-v1[2]*v2[1]);
		float y=-(v1[0]*v2[2]-v1[2]*v2[0]);
		float z=(v1[0]*v2[1]-v1[1]*v2[0]);

		float[] result = {x, y, z, 0};

		return result;
	}

	public void render( GL gl, MaterialCollection materials )
	{
		double p[]={1, 1, 1}; 
		double q[]={0,0,0};
		double r[]={0,0,0};
		double n[]={0,0,0};
		double mat[];
		double l1,l2,l3,s;
		int i,j;

		mat = new double[16];

		n[0] = nx;
		n[1] = ny;
		n[2] = nz;

		l3 = n[0]*n[0] + n[1]*n[1] + n[2]*n[2];
		l2 = n[0]*p[0] + n[1]*p[1] + n[2]*p[2];
		s = l2 / l3;

		q[0] = p[0] - s * n[0]; q[1] = p[1] - s * n[1]; q[2] = p[2] - s * n[2];
		r[0] = n[2] * q[1] - n[1] * q[2];
		r[1] = n[0] * q[2] - n[2] * q[0];
		r[2] = n[1] * q[0] - n[0] * q[1];

		l1 = 0; l2 =0;

		for( i =0; i<3; i++){
			l1 += q[i] * q[i];
			l2 += r[i] * r[i];
		}

		l1 = Math.sqrt(l1);
		l2 = Math.sqrt(l2);
		l3 = Math.sqrt(l3);

		for( i =0; i<3; i++){
			q[i] /= l1;
			r[i] /= l2;
			n[i] /= l3;
		}

		for( i=0;i<16;i++) mat[i] =0;
		mat[15] = 1;

		for( i =0; i<3; i++){
			mat[i+0] = q[i]; 
			mat[i+4] = r[i]; 
			mat[i+8] = n[i]; 
		}

		GLU glu = new GLU();
		GLUquadric quadric = glu.gluNewQuadric();

		gl.glPushMatrix();
		gl.glTranslated(x,y,z);
		gl.glMultMatrixd(mat, 0);
		materials.setMaterial( matId, gl );
		gl.glTranslatef(0f,0f,((float)-length)/2.0f);
		glu.gluCylinder(quadric, radius, radius, length, 20, 1);
		gl.glPushMatrix();
		gl.glRotatef(180,0,1,0);
		glu.gluDisk(quadric, 0.0 , radius, 20, 1);
		gl.glPopMatrix();
		gl.glTranslatef(0,0, ((float)length));
		glu.gluDisk(quadric, 0.0 , radius, 20, 1);
		gl.glPopMatrix();
		glu.gluDeleteQuadric( quadric );
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
		System.out.print( "Cylinder ID:" + id + " mat ID:" + matId);
		System.out.print( " x:" + x + " y:" + y + " z:" + z );
		System.out.print( " nx:" + nx + " ny:" + ny + " nz:" + nz );
		System.out.print( " radius:" + radius + " length:" + length);

		System.out.println();
	}
}

