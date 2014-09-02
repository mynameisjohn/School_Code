/*******************************************************************
  Container class to hold all the Lights 
 *******************************************************************/
import java.util.*;
import javax.media.opengl.*;

public class LightCollection 
{
	private ArrayList<Light> lightArray;

	private final float EPSILON = .000001f;

	public LightCollection()
	{
		lightArray = new ArrayList<Light>();
	}


	public void newLight( StringTokenizer stok )
	{
		int glLightId = 0;

		// glLightId is only relevant for OpenGL
		switch( lightArray.size() )
		{
		case 0: glLightId = GL.GL_LIGHT0; break;
		case 1: glLightId = GL.GL_LIGHT1; break;
		case 2: glLightId = GL.GL_LIGHT2; break;
		case 3: glLightId = GL.GL_LIGHT3; break;
		case 4: glLightId = GL.GL_LIGHT4; break;
		case 5: glLightId = GL.GL_LIGHT5; break;
		case 6: glLightId = GL.GL_LIGHT6; break;
		case 7: glLightId = GL.GL_LIGHT7; break;
		default:
			System.err.println("Exceed max number of lights"); 
			System.exit(1);
		}

		Light light = new Light( stok, glLightId );

		// Check for duplicate light IDs
		for ( Light l : lightArray )
			if ( l.id == light.id )
			{
				System.err.println( "Duplicate ID: " + l.id 
						+ " detected. Object not inserted " );
				return;
			}

		lightArray.add( light );  
	}

	public float[] addLight(Point pI, RenderObjectCollection objects, float[] N, float[] V, float[] rgb, Material M)
	{
		float[] L = new float[4], R=new float[4], lightColors, totalLight=new float[3], pos;
		float[] pInt=pI.toArray();

		for ( Light l : lightArray )
		{
			if (l.isPoint())
			{
				pos=l.getPos();
				for (int i=0;i<3;i++)
					L[i]=pos[i]-pInt[i];
				//Make the rAttenuation more complicated/interesting
				float m=magnitude(L);
				float rAttenuation=1/(m);
				L=Normalize(L);
				float d = dotProduct(N, L);

				if (d>0)
				{//This should ensure the light vector could possibly reach the intersection point
					Ray lRay = new Ray (pI, L);
					float aAttenuation=-dotProduct(L, l.getDir());//Negative because vectors are tip to tail

					for (int i=0;i<4;i++)
						R[i]=(2*d)*N[i]-L[i];
					R=Normalize(R);
					float RV = -dotProduct(R, V);
					lightColors=l.addLight(pI, lRay, objects, N, L);
					float diffuse = M.Kd*d;
					float specular = (float) Math.pow(RV, M.ns)*M.Ks; 
					
					for (int i=0;i<3;i++)
					{//Add specular term later
						totalLight[i]+=(diffuse)*lightColors[i]+specular;
						if (totalLight[i]>1)
							totalLight[i]=1;
					}
				}
			}
			if (l.isAmb())
			{
				lightColors=l.getColor();
				//System.out.println(lightColors[0]+" "+lightColors[1]+" "+lightColors[2]);
				for (int i=0;i<3;i++)
				{
					totalLight[i]+=lightColors[i]*M.Ka;
					if (totalLight[i]>1)
						totalLight[i]=1;
				}
			}

			if (l.isInf())
			{
				float[] dir = Normalize(l.getDir());
				float d=dotProduct(N, dir);
				lightColors=l.getColor();
				for (int i=0;i<3;i++)
				{
					totalLight[i]+=lightColors[i]*d;
					if (totalLight[i]>1)
						totalLight[i]=1;
				}
			}
		}

		for (int i=0;i<3;i++){
			if (totalLight[i]>1)
				totalLight[i]=1;
			if (totalLight[i]<0)
				totalLight[i]=0;
		}
		
		return totalLight;
	}

	public float magnitude (float[] u)
	{
		double magnitude=0; 

		for (int i=0;i<4;i++)
		{
			magnitude+=u[i]*u[i];
		}

		magnitude=Math.sqrt(magnitude);	

		return (float) magnitude;

	}

	public float dotProduct(float[] u, float[] v)
	{
		float a=0;
		if (u.length==v.length)
		{
			for (int i=0;i<u.length;i++)
				a+=u[i]*v[i];
		}
		return a;
	}

	public float[] Normalize (float[] u)
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



	public void setLights( GL gl )
	{
		gl.glEnable( GL.GL_LIGHTING );

		for ( Light l : lightArray)
			l.setLight( gl );
	}

	public void print()
	{
		System.out.println( "---------- LightCollection ----------" );
		for ( Light l : lightArray )
			l.print();
	}
}


