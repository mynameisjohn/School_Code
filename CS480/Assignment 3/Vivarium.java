//************************************************************************
//  Vivarium Class. 
//    This is the main object that coordinates the update among the
//    creatures of the vivarium.
//************************************************************************
// Comments :
//    This is meant for demonstration. PLEASE plan out your own vivarium
//    creatures, display hierarchy, data structures, etc. No teapots 
//    allowed :).
//
// History :
//    5 Mar 2008. Translated from c code by Tai-Peng Tian. Original by
//    Stan Sclaroff.

import javax.media.opengl.*;
import com.sun.opengl.util.*;
import java.util.*;

public class Vivarium
{
  private Tank tank;
  private Teapot teapot;

  public Vivarium()
  {
    tank = new Tank( 4.0f, 4.0f, 4.0f );
    teapot = new Teapot( 0.75f );
  }

  public void init( GL gl )
  {
    tank.init( gl );
    teapot.init( gl );
  }

  public void update( GL gl )
  {
    tank.update( gl );
    teapot.update( gl );
  }

  public void draw( GL gl )
  {
    tank.draw( gl );
    teapot.draw( gl );
  }
}
