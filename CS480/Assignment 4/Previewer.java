/*******************************************************************
   Raytracer Previwer 
 ********************************************************************
History :
1 Apr 08. Created by Tai-peng Tian based on the C++ version created
          by Ashwin Thangali.
 ********************************************************************/

import java.io.*;
import java.util.*;
import java.awt.Color;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*; 

import com.sun.opengl.util.GLUT;

import javax.media.opengl.GL;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.GLCapabilities;
import javax.media.opengl.GLCanvas;
import javax.media.opengl.glu.GLU;
import com.sun.opengl.util.*;

import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;


public class Previewer extends JFrame
implements GLEventListener, KeyListener, MouseListener, MouseMotionListener
{
	private final int DEFAULT_WINDOW_WIDTH=512;
	private final int DEFAULT_WINDOW_HEIGHT=512;
	private final float DEFAULT_LINE_WIDTH=1.0f;

	private final float EPSILON = .000001f;

	private GLCapabilities capabilities;
	private GLCanvas canvas;
	private FPSAnimator animator;
	private GLU glu;
	private GLUT glut;

	private RenderObjectCollection objects;
	private MaterialCollection materials;
	private LightCollection lights;
	private Camera camera;

	Pixel[][] pixelArray;
	float[][] basis;

	public Previewer ( String filename )
	{
		capabilities = new GLCapabilities();
		capabilities.setDoubleBuffered(true);  // Enable Double buffering

		canvas  = new GLCanvas(capabilities);
		canvas.addGLEventListener(this);
		canvas.addMouseListener(this);
		canvas.addMouseMotionListener(this);
		canvas.addKeyListener(this);
		canvas.setAutoSwapBufferMode(true); // true by default. Just to be explicit
		getContentPane().add(canvas);

		animator = new FPSAnimator(canvas, 1); 

		glu  = new GLU();
		glut = new GLUT();

		objects   = new RenderObjectCollection();
		materials = new MaterialCollection();
		lights    = new LightCollection();
		camera    = new Camera();

		readModelFile( filename );

		setTitle("CS480/CS680 Raytracer Previewer");
		setSize( camera.viewportWidth , camera.viewportHeight );
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);

		// // For debugging purpose
		// camera.print();
		// objects.print();
		// lights.print();
		// materials.print();
	}

	public void run()
	{
		animator.start();
		findColors();
	}

	public static void main( String[] args )
	{
		if (args.length < 1) {
			System.out.println( "Usage: viewer <model_file>\n" );
			System.exit(1);
		}

		Previewer P = new Previewer( args[0] );

		P.run();

	}

	//*********************************************** 
	// Parse contents of the file 
	//*********************************************** 
	private void readModelFile( String filename)
	{
		try 
		{
			BufferedReader input = new BufferedReader( new FileReader(filename) );

			try 
			{
				String line = null;
				while ( (line = input.readLine()) != null )
				{
					StringTokenizer stok = new StringTokenizer( line );
					if (stok.hasMoreTokens())
					{
						String tok = stok.nextToken();

						if (tok.equals( "obj" )) objects.newObject( stok );
						else if (tok.equals( "light" )) lights.newLight( stok );
						else if (tok.equals( "mat" )) materials.newMaterial( stok );
						else if (tok.equals( "resolution" )) camera.setResolution( stok );
						else if (tok.equals( "camera" )) camera.setParameters( stok );
						else if (tok.equals( "viewport" )) camera.setViewport( stok );
						else if (tok.equals( "render" )) objects.setRenderObjIds( stok );
					}

				}
			}
			finally
			{
				input.close();
			}

		}
		catch (IOException ex) 
		{
			System.err.println( "File input error" );
		}

		objects.reorganizeObjects(); // remove objects that are not specified 
		// in render option.
		this.pixelArray=new Pixel[camera.viewportHeight][camera.viewportWidth];
	}

	private void draw_axes( float scale, GL gl )
	{


		gl.glDisable( GL.GL_LIGHTING );

		gl.glPushMatrix();
		gl.glScalef( scale, scale, scale );

		gl.glLineWidth(2);
		gl.glBegin( GL.GL_LINES );

		gl.glColor3f(  0.7f, 0f, 0.0f );
		gl.glVertex3f(  .8f, 0.05f, 0.0f );  
		gl.glVertex3f( 1.0f, 0.25f, 0.0f ); /* Letter X */
		gl.glVertex3f( 0.8f, .25f,  0.0f );  
		gl.glVertex3f( 1.0f, 0.05f, 0.0f );
		gl.glVertex3f( 0.0f, 0.0f, 0.0f );  
		gl.glVertex3f( 1.0f, 0.0f, 0.0f ); /* X axis      */

		gl.glColor3f( 0.0f, 0.7f, 0.0f );
		gl.glVertex3f( 0.15f, 0.9f, 0.0f );  
		gl.glVertex3f( 0.15f, 0.8f, 0.0f ); /* Letter Y */
		gl.glVertex3f( 0.15f, 0.9f, 0.0f );  
		gl.glVertex3f( 0.05f, 1.0f, 0.0f );
		gl.glVertex3f( 0.15f, 0.9f, 0.0f );  
		gl.glVertex3f( 0.25f, 1.0f, 0.0f );
		gl.glVertex3f( 0.0f, 0.0f, 0.0f );  
		gl.glVertex3f( 0.0f, 1.0f, 0.0f ); /* Y axis      */

		gl.glColor3f( 0.0f, 0f, 0.7f );
		gl.glVertex3f( 0.05f, 0.0f , 0.8f); 
		gl.glVertex3f( 0.25f, 0.0f , 0.8f); /* Letter Z */
		gl.glVertex3f( 0.05f, 0.0f , 1.0f);  
		gl.glVertex3f( 0.25f, 0.0f , 1.0f);
		gl.glVertex3f( 0.05f, 0.0f , 0.8f);  
		gl.glVertex3f( 0.25f, 0.0f , 1.0f);
		gl.glVertex3f( 0.0f, 0.0f, 0.0f );  
		gl.glVertex3f( 0.0f, 0.0f, 1.0f ); /* Z axis    */
		gl.glEnd();
		gl.glLineWidth(1);
		gl.glPopMatrix();

		gl.glEnable( GL.GL_LIGHTING );

	}

	//*********************************************** 
	//  GLEventListener Interfaces
	//*********************************************** 
	public void init( GLAutoDrawable drawable) 
	{
		GL gl = drawable.getGL();
		gl.glClearColor( 0.0f, 0.0f, 0.0f, 0.0f);

		gl.glEnable( GL.GL_DEPTH_TEST );
		gl.glEnable( GL.GL_NORMALIZE );
		gl.glShadeModel( GL.GL_SMOOTH );

		camera.setCamera( camera.viewportWidth, camera.viewportHeight, gl );

		lights.setLights( gl );
	}

	// Redisplaying graphics
	public void display(GLAutoDrawable drawable)
	{
		GL gl = drawable.getGL();

		/* clear the display */
		gl.glClear( GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT );

		gl.glPushMatrix();
		gl.glTranslatef(0f,0f,0.001f);
		draw_axes(4.5f, gl);
		gl.glPopMatrix();



		objects.render( gl, materials );

	}

	public void findColors()
	{
		double cop[] = camera.getCop();
		float[] u=new float[4];
		float du, dv;
		Ray ray = new Ray();
		Point p = new Point();
		Pixel pixel;
		float[][] basis=camera.defineBasis();
		Pixel[][] AA=new Pixel[3][3];

		for (int y=0;y<camera.viewportHeight;y++)
		{
			for (int x=0;x<camera.viewportWidth;x++)
			{
				for (int i=0;i<3;i++)
				{
					for (int j=0;j<3;j++)
					{
						du=(float) ((x-(camera.viewportWidth/2+1))*camera.xRes-camera.xRes/3+j*camera.xRes/3);
						dv=(float) ((-y+(camera.viewportHeight/2-1))*camera.yRes+camera.yRes/3-i*camera.yRes/3);

						//These are the world locations of each pixel
						p.x=(float) (cop[0]+basis[2][0]*camera.getFocalLength()+du*basis[0][0]+dv*basis[1][0]);
						p.y=(float) (cop[1]+basis[2][1]*camera.getFocalLength()+du*basis[0][1]+dv*basis[1][1]);
						p.z=(float) (cop[2]+basis[2][2]*camera.getFocalLength()+du*basis[0][2]+dv*basis[1][2]);

						//p.print();

						if (camera.isOrthographic())
							u=basis[2];//Set the direction vector to the n, or normalized look, vector. 
						else
						{//Perspective case
							u[0]=(float) (p.x-cop[0]);
							u[1]=(float) (p.y-cop[1]);
							u[2]=(float) (p.z-cop[2]);
						}

						//Set the initial point as the pixel the ray intersects, and the initial direction as the direction it would becoming from the COP
						ray.setPo(p.toArray());
						ray.setVector(u);
						pixel = rayTrace(ray, 0, 0);
						AA[i][j]=pixel;
					}
				}

				//Add this correct pixel to the array in row-column order
				pixelArray[y][x]=antiAlias(AA);
			}
		}
		System.out.println(pixelArray[0].length+" "+pixelArray.length);
		makeImage(pixelArray);
		System.out.println("Done!");


	}

	public Pixel rayTrace( Ray ray, int depth, float t )
	{
		//System.out.println(depth);
		float Kr;
		float[]N, R, L, T, rgb=new float [3], light;
		ArrayList<Pixel> Reflections = new ArrayList<Pixel>();
		Pixel p;
		Point pI;
		Material M;
		RenderObject O;

		//Find the lowest t in the queue, and find its corresponding object. 
		if (depth==0)	
			t=this.objects.findIntersections(ray);



		//System.out.println("t= "+t);

		if (t==4000||t<EPSILON)
		{//Should just return black if nothing hit

			return new Pixel (ray.toPoint(), rgb[0], rgb[1], rgb[2]);
		}

		else
		{
			pI = new Point (ray.u, ray.p0, t);
			O=objects.get_obj_by_id(objects.hitId);
			N=O.returnNormal(pI);
			int m=O.matId;
			M=materials.get_mat_by_id(m);
			Kr=M.Kr;

			/*
			 * At this point we have the point of intersection and the surface normal, 
			 * so we should be able to determine the color at that point. 
			 */

			light=lights.addLight(pI, objects, N, ray.u, rgb, M);
			for (int i=0;i<3;i++)
			{
				rgb[i]+=M.getColor()[i]*light[i];
				if (rgb[i]>1)
					rgb[i]=1;
			}
		
			/*
			 * The reflection and transmission rays are only cast if depth
			 * is within maximum value, say 4. 
			 */
			if (depth<4)
			{
				R = new float[4];
				float NR=dotProduct(N, ray.u);
				for (int i=0;i<4;i++)
					R[i]=(-2*NR)*N[i]+ray.u[i];
				R=Normalize(R);
				Ray rRay = new Ray(pI, R);//u should be found via Snell's law
				t=this.objects.findIntersections(rRay);
				if (t!=4000&&t>EPSILON)
				{
					depth++;
					Reflections.add(rayTrace(rRay, depth, t));
					depth--;
				}

			}
			if (depth>0)
			{
				p=new Pixel (ray.toPoint(), rgb[0]*M.Kr, rgb[1]*M.Kr, rgb[2]*M.Kr);
				return p;
			}
			else
				return computePixel(ray.toPoint(), Reflections, rgb, M.getColor());

		}
	}
	

	public Pixel computePixel (Point p0, ArrayList<Pixel> Reflections, float[] rgb, float[] Mrgb)
	{
		float[] Rrgb=new float[4];
		//Add the colors from the reflections
		for (int i=0;i<Reflections.size();i++)
		{
			Rrgb=Reflections.get(i).color.getColorComponents(Rrgb);
			for (int j=0;j<3;j++)
			{
				rgb[j]+=Rrgb[j]*Mrgb[j];
				if (rgb[j]>1)
					rgb[j]=1;
			}
		}

		return new Pixel (p0, rgb[0], rgb[1], rgb[2]);
	}

	/*
	 * Anti-Aliasing method that divides each pixel into 9 sub-pixels, adding them together with the center pixel having the most weight
	 */
	public Pixel antiAlias(Pixel[][] AA)
	{
		float[] rgb = new float[3];
		float[] p1= new float[3], p2= new float[3], p3= new float[3], p4= new float[3], p5= new float[3], p6= new float[3], p7= new float[3], p8= new float[3], p9= new float[3];

		for (int i=0;i<3;i++)
		{
			rgb[i]+=
					AA[0][0].color.getRGBColorComponents(p1)[i]*.1f+AA[0][1].color.getRGBColorComponents(p2)[i]*.1f+AA[0][2].color.getRGBColorComponents(p3)[i]*.1f+
					AA[1][0].color.getRGBColorComponents(p4)[i]*.1f+AA[1][1].color.getRGBColorComponents(p5)[i]*.2f+AA[1][2].color.getRGBColorComponents(p6)[i]*.1f+
					AA[2][0].color.getRGBColorComponents(p7)[i]*.1f+AA[2][1].color.getRGBColorComponents(p8)[i]*.1f+AA[2][2].color.getRGBColorComponents(p9)[i]*.1f;
		}
		
		Pixel result = new Pixel(0, 0, 0, rgb[0], rgb[1], rgb[2]);
		
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



	// Window size change
	public void reshape(GLAutoDrawable drawable, int x, int y, int w, int h)
	{
		// Change viewport dimensions
		GL gl = drawable.getGL();
	}

	public void displayChanged(GLAutoDrawable drawable, boolean modeChanged,
			boolean deviceChanged)
	{
	}


	//*********************************************** 
	//          KeyListener Interfaces
	//*********************************************** 
	public void keyTyped(KeyEvent key)
	{
		//      Q,q: quit

		switch ( key.getKeyChar() ) {
		case 'Q' :
		case 'q' : new Thread() {
			public void run()
			{ animator.stop(); }
		}.start();
		System.exit(0);
		break;
		default :
			break;
		}
	}

	public void makeImage(Pixel[][] p)
	{

		BufferedImage image = new BufferedImage( p[0].length, p.length, 
				BufferedImage.TYPE_INT_ARGB );

		for (int i=0;i<p.length;i++)
		{
			for (int j=0;j<p[0].length;j++)
			{

				// pack the rgb values into an int
				image.setRGB( j, p.length-i-1, p[i][j].color.getRGB());

				//System.out.println("Made pixel at ("+j+", "+i);
			}
		}
		File outputFile = new File( "rendered_image.png" );
		try {
			javax.imageio.ImageIO.write( image, "PNG", outputFile );
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void keyPressed(KeyEvent key)
	{
		switch (key.getKeyCode()) {
		case KeyEvent.VK_ESCAPE:
			new Thread()
			{
				public void run()
				{
					animator.stop();
				}
			}.start();
			System.exit(0);
			break;
		default:
			break;
		}
	}

	public void keyReleased(KeyEvent key)
	{
	}

	//************************************************** 
	// MouseListener and MouseMotionListener Interfaces
	//************************************************** 
	public void mouseClicked(MouseEvent mouse)
	{
	}

	public void mousePressed(MouseEvent mouse)
	{
	}

	public void mouseReleased(MouseEvent mouse)
	{
	}

	public void mouseMoved( MouseEvent mouse)
	{
	}

	public void mouseDragged( MouseEvent mouse )
	{
	}

	public void mouseEntered( MouseEvent mouse)
	{
	}

	public void mouseExited( MouseEvent mouse)
	{
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


}


