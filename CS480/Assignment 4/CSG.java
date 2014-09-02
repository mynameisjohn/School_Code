
import java.util.*;
import javax.media.opengl.GL;

public class CSG extends RenderObject
{
  private enum CSG_OperationType { UNION, INTERSECTION, DIFFERENCE }
  private CSG_OperationType op;

  private RenderObject obj1, obj2;
  private int obj1_id, obj2_id;

  public int get_obj1_id ()
  { 
    return obj1_id;
  }

  public int get_obj2_id ()
  {
    return obj2_id;
  }

  public void set_obj1( RenderObject obj )
  {
    obj1 = obj;
  }

  public void set_obj2( RenderObject obj )
  {
    obj2 = obj;
  }

  public CSG( StringTokenizer stok, int id_ )
  {
    id = id_;

    String op_str = stok.nextToken();

    if ( op_str.equals( "union" )) 
      op = CSG_OperationType.UNION;
    else if ( op_str.equals( "intersection" )) 
      op = CSG_OperationType.INTERSECTION;
    else if ( op_str.equals( "difference" ))
      op = CSG_OperationType.DIFFERENCE;
    else {
      System.err.println( "Operation " + op_str + " not supported." );
      System.exit(1);
    }

    stok.nextToken(); // Strip away the token "ID"
    obj1_id = Integer.parseInt( stok.nextToken() );

    stok.nextToken(); // Strip away the token "ID"
    obj2_id = Integer.parseInt( stok.nextToken() );
  }

  public void render( GL gl, MaterialCollection materials )
  {
    // Do nothing as CSG not supported in this viewer
    System.out.println( "CSG.render() not supported" );
  }

  public void print()
  {
    System.out.print( "CSG ID:" + id );
    System.out.print( " obj1 ID:" + obj1_id );
    System.out.print( " obj2 ID:" + obj2_id );
    System.out.println();

  }

}

