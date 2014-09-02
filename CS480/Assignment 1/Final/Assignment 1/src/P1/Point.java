/**
 * Point.java
 */
package P1;

/**
 * A two-dimensional vector with floating point values.
 * 
 * @author Stan Sclaroff <sclaroff>
 * @author Tai-Peng Tian <tiantp@gmail.com>
 * @author Jeffrey Finkelstein <jeffreyf>
 */
class Point {
  /** The x and y values. */
  public float x, y;

  /**
   * Instantiates this vector with the specified initial values.
   * 
   * @param x
   *          The initial x value of this vector.
   * @param y
   *          The initial y value of this vector.
   */
  public Point(final float x, final float y) {
    this.x = x;
    this.y = y;
  }

  /**
   * Returns a String representation of this vector.
   * 
   * @return A String representation of this vector.
   */
  @Override
  public String toString() {
    return "(" + this.x + ", " + this.y + ")";
  }
}
