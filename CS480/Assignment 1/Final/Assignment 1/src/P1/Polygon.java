// ****************************************************************************
// Polygon class.
// ****************************************************************************
// Comments :
// Subroutines to manage and draw polygons
//
// History :
// 7 February 2011 - updated code to be more Java-y
// 9 Jan 2008 Created by Tai-Peng Tian (tiantp@gmail.com) based on code by
// Stan Sclaroff (from CS480 '06 poly.c)
package P1;

import java.util.ArrayList;
import java.util.List;

/**
 * A polygon with additional drawing capabilities.
 * 
 * @author Stan Sclaroff <sclaroff>
 * @author Tai-Peng Tian <tiantp@gmail.com>
 * @author Jeffrey Finkelstein <jeffreyf>
 */
public class Polygon {

	/** The currently selected vertex, which will be moved. */
	private Point selectedVertex = null;
	/** The list of vertices that make up this polygon. */
	public final ArrayList<Point> vertices = new ArrayList<Point>();
	/** The list of edge vectors that make up the polygon */

	/** Booleans that activate if the vertices are entered in a clockwise/counterclockwise fashion. */
	public boolean clockWise = false;
	public boolean counterClockwise = false;

	//public final float[] extremes = extremes();
	/** The polygon's RGB color, which can be changed */

	private Color color = new Color();

	public Polygon()
	{
		// default constructor gives polygon color = white
		this.setRGBColor(1, 1, 1);
	}

	public Polygon(float r, float g, float b)
	{
		this.setRGBColor(r, g, b);
	}

	/**
	 * Add a vertex to this polygon at the point specified by the given x and y
	 * values.
	 * 
	 * @param x
	 *          The x value of the vertex to add.
	 * @param y
	 *          The y value of the vertex to add.
	 */
	public void addVert(final int x, final int y) {
		this.vertices.add(new Point(x, y));
	}

	/**
	 * Returns {@code true} if and only if this polygon is concave.
	 * 
	 * @return {@code true} if and only if this polygon is concave.
	 */
	public boolean isConcave() {
		/**
		 * This method determines whether or not the polygon is concave. The trademark of concavity is a discrepancy
		 * in the sign of the cross product of edge vectors. If two edges form an obtuse interior angle, the cross product
		 * is will be the opposite of what it would be if they formed an acute angle. The method finds the cross product of 
		 * the edge vectors, and if any two have a different sign it returns true.
		 */

		boolean pos=false, neg=false; //Booleans that are turned true if the cross product of two edges has a z component that is positive or negative
		float z; //The z component of the cross product. 
		int CW=0, CCW=0;//Clockwise and Counterclockwise. 
		//Integers that are incremented depending on the sign of z, which are tallied to determine the overall direction of entry.
		ArrayList<float[]> edgeVectors = computeEdgeVectors();

		for (int i=0;i<this.vertices.size();i++)
		{
			if (i==this.vertices.size()-1)//This is the last case handled. 
			{
				z = computeXproduct(edgeVectors.get(i), edgeVectors.get(0))[2];
			}
			else
			{
				z = computeXproduct(edgeVectors.get(i), edgeVectors.get(i+1))[2];
			}
			//If the change in angle between vectors that would indicate concavity occurs, the booleans are changed. 
			if (z>0) {pos=true; CW++;}
			if (z<0) {neg=true; CCW++;}
			if (CW>CCW)
			{//This method seems a bit barbaric. I'm curious about what happens when the two integers are equal. 
				this.clockWise = true;
				this.counterClockwise = false;
			}
			else
			{
				this.counterClockwise = true;
				this.clockWise = false;
			}
			if (pos&&neg)
			{//If both booleans have been turned  true, the polygon is concave.  
				return true;
			}
		}

		return false;
	}


	/**A method I wrote to calculate the edge vectors between all the vertices of the Polygon and return them in an array. 
	 * The array size is the number of vertices for any polygon, which made for loops involving both relatively simple. */
	public ArrayList<float[]> computeEdgeVectors()
	{

		ArrayList<float[]> edgeVectors = new ArrayList<float[]>();


		for (int i=0;i<this.vertices.size();i++)
		{
			float[] vector = new float[3];
			if (i==this.vertices.size()-1) //The last case to be added
			{
				vector[0] = this.vertices.get(0).x-this.vertices.get(i).x;
				vector[1] = this.vertices.get(0).y-this.vertices.get(i).y;
				vector[2] = 0; //There is no z component in the Point class
			}
			else 
			{
				vector[0] = this.vertices.get(i+1).x-this.vertices.get(i).x;
				vector[1] = this.vertices.get(i+1).y-this.vertices.get(i).y;
				vector[2] = 0; //There is no z component in the Point class
			}
			edgeVectors.add(vector);
		}
		return edgeVectors;
	}

	/**A method I wrote that computes the cross product of two three dimensional vectors, returning them as a 3-D vector (array). */
	public float[] computeXproduct (float[] v1, float[] v2)
	{
		float x=(v1[1]*v2[2]-v1[2]*v2[1]);
		float y=(v1[0]*v2[2]-v1[2]*v2[0]);
		float z=(v1[0]*v2[1]-v1[1]*v2[0]);
		float[] result = {x, y, z};
		return result;
	}



	/**
	 * Returns {@code true} if and only if this polygon overlaps p.
	 * 
	 * @param p
	 *          The polygon to be tested for overlap 
	 *          
	 * @return {@code true} if and only if this polygon overlaps p.
	 */

	public boolean isOverlapping(Polygon p){
		/**
		 * Test to see if this polygon overlaps with a given polygon p. 
		 * For now this always returns false. You should implement this method.
		 */

		/*
		for (int i=0;i<p.vertices.size();i++){
			if (this.isInside(p.vertices.get(i).x, p.vertices.get(i).y))
				return true;} This method works, but polygons can overlap without having vertices inside each other. */

		ArrayList<float[]> edgeVectors0 = computeEdgeVectors();
		ArrayList<float[]> edgeVectors1 = p.computeEdgeVectors();

		for (int i=0;i<this.vertices.size();i++)
		{
			for (int j=0;j<p.vertices.size();j++)
			{
				float[] poi = intersection(this.vertices.get(i).x, this.vertices.get(i).y, p.vertices.get(j).x, p.vertices.get(j).y, edgeVectors0.get(i), edgeVectors1.get(j));

				if (poi[0]!=-1)
				{
					return true;
				}
			}
		}	



		return false;


	}



	/**
	 * Returns {@code true} if and only if the specified point is inside this
	 * polygon.
	 * 
	 * @param x
	 *          The x value of the point to test.
	 * @param y
	 *          The y value of the point to test.
	 * @return {@code true} if and only if the specified point is inside this
	 *         polygon.
	 */
	public boolean isInside(final float x, final float y) {
		/**
		 * For now, this method always returns false. Remove this line when you
		 * implement your own algorithm.
		 */

		//Simple boundary box test, I figured it would save some calculations. 
		float[] extremes = this.extremes();
		if (x<=extremes[0]||y<=extremes[1]||x>=extremes[2]||y>=extremes[3])
		{
			return false;
		}
		else
		{
			ArrayList<float[]> edgeVectors = computeEdgeVectors();

			int n=0;//Variable used to count the number of intersections
			float[] vtest = {-x, 0};//The test vector is just a horizontal vector from the given point to x=0
			for (int i=0;i<edgeVectors.size();i++)
			{
				//if (edgeVectors.get(i)[1]!=0)
				{
					float[] point = intersection(this.vertices.get(i).x, this.vertices.get(i).y, x, y,edgeVectors.get(i), vtest);
					//System.out.println(point[0]+", "+point[1]);
					if (point[0]!=-1&&point[0]<x&&point[1]!=this.vertices.get(i).y)//&&edgeVectors.get(i)[1]!=0)//-1 Should only be returned if there is no intersection (I think this is where the problem is)
						n++;
				}
			}

			if (n%2==1)//If the number of intersections is odd
				return true;
		}

		return false;
	}

	public static float[] intersection (float x0, float y0, float x1, float y1, float[] v0, float[] v1)
	{
		//x1 and y1 should be test points, and v2 should be the test vector
		//System.out.println("checking points "+x0+", "+y0+", "+x1+", "+y1+" vectors: "+v0[0]+", "+v0[1]+" and "+v1[0]+", "+v1[1]);

		float[] point = new float[2];
		float u, v;//Two variables that add the vector to a given point

		//u=(y1*v1[0]+x0*v1[1]-x1*v1[1]-y0*v1[0])/(v0[1]*v1[0]-v0[0]*v1[1]); Old formula I used. I had a lot of trouble getting this right, and I'm not really sure why. The algebra seemed right to me in both cases. 

		//Formula worked out by solving two equations for two unknowns, in this case u and v. 
		u=((x1-x0)*v1[1]+v1[0]*y0-v1[0]*y1)/(v0[0]*v1[1]-v1[0]*v0[1]);
		v=(y0+u*v0[1]-y1)/v1[1];

		point[0]=x0+u*v0[0];
		point[1]=y0+u*v0[1];

		if (u<=0||v<=0||u>1||v>1){//If the intersection point is out of the range of the edge vector. 
			point[0]=-1;//For some reason this still gives me some bugs. I experimented a lot with this intersection method, and logically this way makes the most sense to me. 
			point[1]=-1;//However, in some test cases there are bugs with the inside outside test that I can't seem to explain. 
		}
		return point;
	}

	/**A method I wrote to calclulate the "boundary box" coordinates for a polygon. 
	 * That is, the extreme x and y values that form a quadrilater around the polygon in the form
	 * {xmin, ymin, xmax, ymax}
	 * @return
	 */
	public float[] extremes()
	{  //This initial values are arbitrarily high so they will be changed by the input values from the polygon. 
		float[] extremes = {513, 513, 0, 0};
		for (int i=0;i<vertices.size();i++)
		{	//Values are changed accordingly. 
			if (vertices.get(i).x<extremes[0])
			{extremes[0]=vertices.get(i).x;}
			if (vertices.get(i).y<extremes[1])
			{extremes[1]=vertices.get(i).y;}
			if (vertices.get(i).x>extremes[2])
			{extremes[2]=vertices.get(i).x;}
			if (vertices.get(i).y>extremes[3])
			{extremes[3]=vertices.get(i).y;}
		}
		return extremes;
	}


	/**
	 * Moves the currently selected vertex to the specified location.
	 * 
	 * @param x
	 *          The x value of the point to which to move the currently selected
	 *          vertex.
	 * @param y
	 *          The y value of the point to which to move the currently selected
	 *          vertex.
	 */
	public void moveVert(final int x, final int y) {
		if (this.selectedVertex != null) {
			this.selectedVertex.x = x;
			this.selectedVertex.y = y;
		}
	}

	/**
	 * Resets this polygon so that its list of vertices is empty and the currently
	 * selected vertex is {@code null}.
	 */
	public void reset() {
		this.selectedVertex = null;
		this.vertices.clear();
	}

	/**
	 * Selects the vertex closest to the specified point.
	 * 
	 * @param x
	 *          The x value of the point used to select the nearest vertex.
	 * @param y
	 *          The y value of the point used to select the nearest vertex.
	 */
	public void selectVert(final int x, final int y) {
		if (this.vertices.isEmpty())
			return;
		final Point c = this.vertices.get(0);
		float dx = x - c.x;
		float dy = y - c.y;
		float winning_dist_squared = dx * dx + dy * dy;
		Point winner = c;
		for (final Point vertex : this.vertices) {
			dx = x - vertex.x;
			dy = y - vertex.y;
			float dist_squared = dx * dx + dy * dy;
			if (dist_squared < winning_dist_squared) {
				winner = vertex;
				winning_dist_squared = dist_squared;
			}
		}
		this.selectedVertex = winner;
	}

	/**
	 * Returns the String representation of this polygon.
	 * 
	 * @return The String representation of this polygon.
	 */
	@Override
	public String toString() {
		String result = "Polygon[";
		for (final Point vertex : this.vertices) {
			result += vertex + ",";
		}
		result = result.substring(0, result.length() - 1);
		result += "]";
		return result;
	}

	/**
	 * Returns the list of vertices which comprise this polygon.
	 * 
	 * @return The list of vertices which comprise this polygon.
	 */
	List<Point> vertices() {
		return this.vertices;
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		this.color = color;
	}
	public void setRGBColor(float r, float g, float b) {
		this.color.setColor(r, g, b);
	}
	

	/**This is a method I tried to make that would update a class constant created
	 **for the edge vectors. It never worked, and I didn't have time to fix it, but
	 **I think it would be useful for future projects. 
	 */
	/*
	public void addEdgeVector(final int x, final int y)
	{


			  float[] vector = new float[3];
			  if (this.vertices.size()==3)
			  { //Initialization of edge vectors
				  vector[0]=this.vertices.get(1).x-this.vertices.get(0).x;
				  vector[1]=this.vertices.get(1).y-this.vertices.get(0).y;
				  vector[2]=0;//There is no z component in the Point class
				  this.edgeVectors.add(vector);
				  vector[0]=this.vertices.get(2).x-this.vertices.get(1).x;
				  vector[1]=this.vertices.get(2).y-this.vertices.get(1).y;
				  vector[2]=0;//There is no z component in the Point class
				  this.edgeVectors.add(vector);
				  vector[0]=this.vertices.get(0).x-this.vertices.get(2).x;
				  vector[1]=this.vertices.get(0).y-this.vertices.get(2).y;
				  vector[2]=0;//There is no z component in the Point class
				  this.edgeVectors.add(vector);
			  }
			  else
			  { //All other cases
				  vector[0]=x-this.vertices.get(this.vertices.size()-1).x-this.vertices.get(0).x;
				  vector[1]=y-this.vertices.get(this.vertices.size()-1).y-this.vertices.get(0).y;
				  vector[2]=0;//There is no z component in the Point class
				  this.edgeVectors.set(this.vertices.size()-2, vector);
				  vector[0]=this.vertices.get(0).x-x;
				  vector[1]=this.vertices.get(0).y-y;
				  vector[2]=0;//There is no z component in the Point class
				  this.edgeVectors.add(vector);
			  }
	}*/

}
