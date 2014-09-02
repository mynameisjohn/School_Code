/**
 * TestCases.java
 */
package P1;

import java.util.Iterator;

/**
 * An iterator over polygon test cases.
 * 
 * @author Jeffrey Finkelstein <jeffreyf>
 * @author Stan Sclaroff <sclaroff>
 * @since Spring 2011
 * 
 */
public class TestCases implements Iterator<Polygon> {

  /** The total number of test cases. */
  public static final int NUM_TEST_CASES = 10;

  /** The current test case. */
  private int currentTestCase = 0;

  /**
   * Always returns {@code true}.
   * 
   * @return {@code true}
   * @see java.util.Iterator#hasNext()
   */
  @Override
  public boolean hasNext() {
    return true;
  }

  /**
   * Gets a new polygon which is the next test case.
   * 
   * Cycles through {@value #NUM_TEST_CASES} test cases.
   * 
   * @return A new polygon which is the next test case.
   * @see java.util.Iterator#next()
   */
  @Override
  public Polygon next() {
    final Polygon polygon = new Polygon();

    switch (this.currentTestCase) {
    case 0:
    	polygon.setRGBColor((float)0.0, (float)1.0, (float)0.8);
    	polygon.addVert(50, 50);
    	polygon.addVert(250, 50);
    	polygon.addVert(250, 200);
    	polygon.addVert(150, 125);
    	polygon.addVert(50, 200);
    	break;
    	
    case 1:
    	polygon.setRGBColor((float)1.0, (float)0.0, (float)0.2);
    	polygon.addVert(60, 60);
    	polygon.addVert(60, 110);
    	polygon.addVert(110, 110);
    	polygon.addVert(110, 60);
    	break;

    case 2:
    	polygon.setRGBColor((float)0.5, (float)0.1, (float)1.0);
    	polygon.addVert(50, 200);
    	polygon.addVert(50, 50);
    	polygon.addVert(200, 50);
    	polygon.addVert(200, 100);
    	polygon.addVert(100, 100);
    	polygon.addVert(100, 200);
    	break;

    case 3:
    	polygon.setRGBColor((float)0.2, (float)0.8, (float)0.3);
    	polygon.addVert(200, 200);
    	polygon.addVert(130, 200);
    	polygon.addVert(130, 120);
    	polygon.addVert(200, 120);
    	break;

    case 4:
    	polygon.setRGBColor((float)0.8, (float)0.2, (float)0.3);
    	polygon.addVert(50, 100);
    	polygon.addVert(100, 100);
    	polygon.addVert(100, 50);
    	polygon.addVert(150, 50);
    	polygon.addVert(150, 100);
    	polygon.addVert(200, 100);
    	polygon.addVert(200, 150);
    	polygon.addVert(150, 150);
    	polygon.addVert(150, 200);
    	polygon.addVert(100, 200);
    	polygon.addVert(100, 150);
    	polygon.addVert(50, 150);
    	break;

    case 5:
    	polygon.setRGBColor((float)0.1, (float)0.5, (float)1.0);
    	polygon.addVert(50, 250);
    	polygon.addVert(50, 150);
    	polygon.addVert(100, 150);
    	polygon.addVert(150, 50);
    	polygon.addVert(200, 200);
    	polygon.addVert(250, 50);
    	polygon.addVert(275, 150);
    	polygon.addVert(275, 250);
    	break;

    case 6:
      	polygon.setRGBColor((float)0.1, (float)1.0, (float)1.0);polygon.addVert(50, 50);
      	polygon.addVert(50, 200);
      	polygon.addVert(75, 200);
      	polygon.addVert(75, 100);
      	polygon.addVert(100, 100);
      	polygon.addVert(100, 200);
      	polygon.addVert(125, 200);
      	polygon.addVert(125, 100);
      	polygon.addVert(150, 100);
      	polygon.addVert(150, 200);
      	polygon.addVert(175, 200);
      	polygon.addVert(175, 100);
      	polygon.addVert(275, 100);
      	polygon.addVert(275, 5);
      	polygon.addVert(250, 5);
      	polygon.addVert(250, 50);
      	polygon.addVert(225, 50);
      	polygon.addVert(225, 5);
      	polygon.addVert(200, 5);
      	polygon.addVert(200, 50);
      	polygon.addVert(175, 50);
      	polygon.addVert(175, 5);
      	polygon.addVert(150, 5);
      	polygon.addVert(150, 50);
      	break;

    case 7:
      	polygon.setRGBColor((float)0.8, (float)0.8, (float)0.2);
      	polygon.addVert(39, 272);
    	polygon.addVert(44, 215);
    	polygon.addVert(83, 215);
    	polygon.addVert(83, 163);
    	polygon.addVert(95, 163);
    	polygon.addVert(95, 240);
    	polygon.addVert(130, 240);
    	polygon.addVert(130, 168);
    	polygon.addVert(145, 168);
    	polygon.addVert(145, 245);
    	polygon.addVert(191, 245);
    	polygon.addVert(191, 140);
    	polygon.addVert(270, 110);
    	polygon.addVert(269, 281);	
      break;

    case 8:
    	polygon.setRGBColor((float)1.0, (float)0.9, (float)0.55);
    	polygon.addVert(108, 111); 
    	polygon.addVert(140, 147); 
    	polygon.addVert(169, 167); 
    	polygon.addVert(225, 111); 
    	polygon.addVert(181, 106);
    	break;

    case 9:
    	polygon.setRGBColor((float)0.9, (float)0.55, (float)1.0);
      polygon.addVert(9, 260);
      polygon.addVert(96, 114);
      polygon.addVert(168, 178);
      polygon.addVert(242, 99);
      polygon.addVert(16, 99);
      polygon.addVert(13, 20);
      polygon.addVert(279, 24);
      polygon.addVert(279, 261);
      break;
    }

    this.currentTestCase = (this.currentTestCase + 1) % NUM_TEST_CASES;

    return polygon;
  }

  /**
   * Always throws a {@link UnsupportedOperationException}.
   * 
   * @throws UnsupportedOperationException
   *           Always throws this exception.
   * @see java.util.Iterator#remove()
   */
  @Override
  public void remove() {
    throw new UnsupportedOperationException();
  }
}