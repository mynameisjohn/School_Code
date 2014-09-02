/*******************************************************************
  Object class is an abstract class used to derive other 3D primitives.
  Ellipsoid, Sphere, Box classes derive from Object class, they read 
	and store the parameters for the corresponding 3D primitive.
  CSG class derives from Object.
  Objects class stores an array of Object instances.
 ********************************************************************/

import java.util.*;
import javax.media.opengl.*;

public class RenderObjectCollection 
{
	public ArrayList<RenderObject> objectsArray;
	public ArrayList<RenderObject> Q_1_objectsArray;
	public ArrayList<RenderObject> Q_2_objectsArray;
	public ArrayList<RenderObject> Q_3_objectsArray;
	public ArrayList<RenderObject> Q_4_objectsArray;
	public ArrayList<Integer> renderObjectsId;

	private boolean render_all;

	public int hitId;

	public RenderObjectCollection()
	{
		objectsArray = new ArrayList<RenderObject>();
		Q_1_objectsArray = new ArrayList<RenderObject>();
		Q_2_objectsArray = new ArrayList<RenderObject>();
		Q_3_objectsArray = new ArrayList<RenderObject>();
		Q_4_objectsArray = new ArrayList<RenderObject>();
		renderObjectsId = new ArrayList<Integer>();
		render_all = true;
	}

	public void sortSpace()
	{
		for (RenderObject ro:objectsArray)
		{
			if ((ro.returnX()>=0)&&(ro.returnY()>=0))
				Q_1_objectsArray.add(ro);
			else if ((ro.returnX()<0)&&(ro.returnY()>=0))
				Q_2_objectsArray.add(ro);
			else if ((ro.returnX()<0)&&(ro.returnY()<0))
				Q_3_objectsArray.add(ro);
			else if ((ro.returnX()>=0)&&(ro.returnY()<0))
				Q_4_objectsArray.add(ro);
		}
	}

	public void newObject( StringTokenizer stok )
	{
		int i, j;

		String type = stok.nextToken(); 

		stok.nextToken(); // strip away the token "ID"
		int id = Integer.parseInt( stok.nextToken() );

		RenderObject obj;

		if ( type.equals( "sphere" ))  obj = new Sphere( stok, id );
		else if (type.equals( "box" )) obj = new Box( stok, id );
		else if (type.equals( "cylinder" ))  obj = new Cylinder(stok, id );
		else if (type.equals( "ellipsoid" )) obj = new Ellipsoid(stok, id );
		else if (type.equals( "CSG" )) {

			CSG csg = new CSG( stok, id );

			// Setup the reference to the correct parts pointed by the component id 
			RenderObject obj1 = get_obj_by_id( csg.get_obj1_id() );
			if ( obj1 != null)
				csg.set_obj1( obj1);
			else {
				System.err.println( "CSG Component ID " + csg.get_obj1_id() + 
						" has not been defined ");
				System.exit(1);
			}

			RenderObject obj2 = get_obj_by_id( csg.get_obj2_id() );
			if ( obj2 != null)
				csg.set_obj2( obj2);
			else {
				System.err.println( "CSG Component ID " + csg.get_obj2_id() + 
						" has not been defined ");
				System.exit(1);
			}

			obj =  csg;
		}
		else 
		{
			System.err.println( "Object type " + type + " is not supported " );
			return;
		}

		// Check for duplicate object ids
		for ( RenderObject ro : objectsArray )
			if (ro.id == obj.id)
			{
				System.err.println( "Duplicated ID : " + ro.id 
						+ " detected. Object not inserted " );
				return;
			}

		if (obj.returnX()>0&&obj.returnY()>0)
		{

		}

		objectsArray.add( obj );
		
		

		// System.out.println( "Objects.newObject" );
	}

	public void setRenderObjIds( StringTokenizer stok )
	{
		int num_items = Integer.parseInt( stok.nextToken() );

		if (num_items == 0 )
			render_all = true;
		else
		{
			render_all = false; 

			for (int i = 0; i<num_items; ++i )
			{
				stok.nextToken(); // strip away the token "ID"
				renderObjectsId.add( Integer.parseInt(stok.nextToken()) );
			}
		}
	}

	// Remove objects that are not listed in the renderObjectsId array 
	public void reorganizeObjects()
	{
		int i = 0;
		if (!render_all)
		{
			ArrayList<RenderObject> temp = objectsArray;
			objectsArray = new ArrayList<RenderObject>();

			for (RenderObject ro : temp)
				if ( renderObjectsId.contains( ro.id ) ) 
					objectsArray.add( ro ); 
		}
	}

	public void render( GL gl, MaterialCollection materials )
	{
		for ( RenderObject ro : objectsArray )
		{
			ro.render( gl, materials );
		}
	}

	public float findIntersections (Ray ray)
	{
		float t=4000, t0;
		for ( RenderObject ro : objectsArray )
		{
			//System.out.println(ro.id);
			t0=ro.findIntersections (ray);
			if (t0<t)
			{
				//System.out.println("derp");
				t=t0;
				this.hitId=ro.id;
			}
		}
		//System.out.println(t);
		return t;
	}
	/*
	public ArrayList<RenderObject> findArray(Ray ray)
	{
		
		
		if ((ro.returnX()>=0)&&(ro.returnY()>=0))
			Q_1_objectsArray.add(ro);
		else if ((ro.returnX()<0)&&(ro.returnY()>=0))
			Q_2_objectsArray.add(ro);
		else if ((ro.returnX()<0)&&(ro.returnY()<0))
			Q_3_objectsArray.add(ro);
		else if ((ro.returnX()>=0)&&(ro.returnY()<0))
			Q_4_objectsArray.add(ro);
	}
*/
	public RenderObject get_obj_by_id( int id )
	{
		RenderObject result = null;

		for ( RenderObject ro : objectsArray )
			if ( ro.id == id )
			{
				result = ro;
				break;
			}

		return result;
	}

	public void print()
	{
		System.out.println( "--------- RenderObjectCollection ----------" );
		for ( RenderObject ro : objectsArray )
			ro.print();
	}

}


