/**
 * Dog.java - dog model which can draw itself in OpenGL
 * 
 * History:
 * 
 * 18 February 2011
 * 
 * - added documentation
 * 
 * (Jeffrey Finkelstein <jeffrey.finkelstein@gmail.com>)
 * 
 * 16 January 2008
 * 
 * modified to dog class by Stan Sclaroff in February, 2012
 * 
 * (Stan Sclaroff <sclaroff@cs.bu.edu>)
 */


import javax.media.opengl.GL;

import java.util.*;



import com.sun.opengl.util.GLUT;

/**
 * A model of a dog containing an ellipsoid for the body, four Limbs, a neck, head, and tail.
 * 
 * This class provides methods for rotating individual joints on individual
 * Limbs as well.
 * 
 * @author Stan Sclaroff
 * @since Spring 2012
 */
public class Dog {

	/** The x-, y-, or z-axis. */
	enum AxisType {
		X, Y, Z
	}

	/** The body part to toggle. */
	enum BodyPartType {
		FRONT_LEFT_LEG, FRONT_RIGHT_LEG, REAR_LEFT_LEG, REAR_RIGHT_LEG, NECK, HEAD, TAIL
	}

	/** The type of joint at which to enable rotation. */
	enum JointType {
		PAW, UPPER, LOWER
	}

	enum dogPosition {
		DEFAULT, SIT, ROLL_OVER, BEG, JUMP, DOWN
	}


	/**
	 * The change in rotation angle (in degrees) to apply on each rotation update.
	 */
	private double[] DELTA_ANGLE = new double[3];

	/** The currently active parts. */
	private final boolean active_parts[] = new boolean[] { false, false,
			false, false, false, false, false };
	/** The currently active joint type. */
	private JointType active_joint = JointType.PAW;
	/** The currently active axis of rotation. */
	private AxisType active_rotation_axis = AxisType.X;
	/** The OpenGL handle (integer) for the OpenGL call list object. */
	private int dog_positions[] = new int[7];

	private int active_limb;
	private int active_axis;

	private int current_position;

	final private Limb[] Limbs = new Limb[7];

	private float body_rotation;




	/**
	 * Whether the state of the dog model has changed since the last update.
	 * 
	 * This is initially true so that the dog will show up initially.
	 */
	private boolean state_has_changed = true;

	private final GLUT glut;

	/**
	 * Instantiates this dog with access to the specified OpenGL utility toolkit
	 * object.
	 * 
	 * @param glut
	 *          The OpenGL utility toolkit object.
	 */
	public Dog(final GLUT glut) {
		this.glut = glut;

		Limbs[0] = new Limb (-1, -.4125f, .6f);
		Limbs[1] = new Limb (1, -.4125f, .6f);
		Limbs[2] = new Limb (-1, -.4125f, -.6f);
		Limbs[3] = new Limb (1, -.4125f, -.6f);
		Limbs[4] = new Limb (.5f, 0, 0);
		Limbs[5] = new Limb (-1.5f, 0, 0);
		Limbs[6] = new Limb (1.25f, .75f, 0);

		this.current_position = 0;
		this.body_rotation = 0;

		for (int i=0;i<3;i++)
			this.DELTA_ANGLE[i]=0;

		for (int i=0;i<6;i++)
			this.dog_positions[i]=i;
	}

	/**
	 * Decreases the rotation of the currently selected joint on the currently
	 * selected body parts by {@value #DELTA_ANGLE} degrees.
	 */
	public void rotateActiveJointsForward() {
		this.state_has_changed = true;// flag to recreate the display list

		switch (active_joint) {

		case PAW:
			while (this.Limbs[this.active_limb].paw_angle[this.active_axis]<=10){
				this.Limbs[this.active_limb].paw_angle[this.active_axis]++;
				System.out.println("Dog::decrement_rotation_angle()");
				break;}
			break;

		case UPPER:
			while (this.Limbs[this.active_limb].upper_angle[this.active_axis]<=30){
				this.Limbs[this.active_limb].upper_angle[this.active_axis]++;
				System.out.println("Dog::decrement_rotation_angle()");
				break;}
			break;

		case LOWER:
			while (this.Limbs[this.active_limb].lower_angle[this.active_axis]<=30){
				this.Limbs[this.active_limb].lower_angle[this.active_axis]++;

				System.out.println("Dog::decrement_rotation_angle()");
				break;}
			break;
		}



		// you will need to rewrite this function

	}

	public void Jump()
	{
		for (int i=0;i<3;i++){
			this.body_rotation++;
			this.state_has_changed = true;
			wait(.5);
		}
	}

	public static void wait (double n){
		long t0,t1;
		t0=System.currentTimeMillis();
		do{
			t1=System.currentTimeMillis();
		}
		while ((t1 - t0) < (n * 1000));
	}

	/**
	 * Draws the god on the specified OpenGL object based on the current state.
	 * 
	 * @param gl
	 *          The GL object with which to draw the dog.
	 */


	public void draw(final GL gl) {

		gl.glCallList(this.dog_positions[current_position]);
	}

	public void togglePosition()
	{
		switch (current_position)
		{
		case 0:
			current_position = 1;
			System.out.println("Mode: Sitting");
			break;

		case 1:
			current_position = 2;
			System.out.println("Mode: Begging");
			break;

		case 2:
			current_position = 3;
			System.out.println("Mode: Laying Down");
			break;

		case 3:
			current_position = 4;
			System.out.println("Mode: Roll Over");
			break;

		case 4:
			current_position = 5;
			reInit();
			System.out.println("Mode: Jump");
			break;

		case 5:
			current_position = 6;
			reInit();
			System.out.println("Mode: High 5");
			break;

		case 6:
			current_position = 0;
			reInit();
			System.out.println("Mode: Controllable Dog");
			break;
		}
	}


	/**
	 * Increases the rotation of the currently selected joint on the currently
	 * selected body parts by {@value #DELTA_ANGLE} degrees.
	 */
	public void rotateActiveJointsBackward() {
		this.state_has_changed = true;// flag to recreate the display list

		switch (active_joint){
		case PAW:
			while (this.Limbs[this.active_limb].paw_angle[this.active_axis]>=-10){
				this.Limbs[this.active_limb].paw_angle[this.active_axis]--;
				System.out.println("Dog::decrement_rotation_angle()");
				break;
			}
			break;

		case UPPER:
			while (this.Limbs[this.active_limb].upper_angle[this.active_axis]>=-30){
				this.Limbs[this.active_limb].upper_angle[this.active_axis]--;
				System.out.println("Dog::decrement_rotation_angle()");
				break;
			}
			break;

		case LOWER:
			while (this.Limbs[this.active_limb].lower_angle[this.active_axis]>=-30){
				this.Limbs[this.active_limb].lower_angle[this.active_axis]--;
				System.out.println("Dog::decrement_rotation_angle()");
				break;
			}
			break;
		}
		// you will need to rewrite this function

	}

	/**
	 * Uses the specified OpenGL object to create a new OpenGL call list on which
	 * to draw the dog's body, Limbs, tail, head, and neck.
	 * 
	 * @param gl
	 *          The OpenGL object from which to get a handle (which is just a
	 *          unique integer) for a call list.
	 */
	public void init(final GL gl) {
		this.dog_positions[0] = gl.glGenLists(20);
		for (int i=this.dog_positions[0];i<6;i++)
			dog_positions[i]=dog_positions[0]+i;
		this.update(gl);
	}

	/**
	 * Activates the specified type of joint for rotation.
	 * 
	 * @param joint
	 *          The type of joint to rotate.
	 */
	public void set_joint(final JointType joint) {
		switch (joint) {
		case PAW:
			System.out.println(" Paw Chosen ");
			break;
		case UPPER:
			System.out.println(" Upper Chosen ");
			break;
		case LOWER:
			System.out.println(" Lower Chosen ");
			break;
		}
		this.active_joint = joint;
	}

	/**
	 * Sets the axis around which to rotate.
	 * 
	 * @param axis
	 *          The axis around which to rotate.
	 */
	public void set_rotation_axis(AxisType axis) {
		switch (axis) {
		case X:
			System.out.println("X-axis chosen");
			this.active_axis = 0;
			break;
		case Y:
			System.out.println("Y-axis chosen");
			this.active_axis = 1;
			break;
		case Z:
			System.out.println("Z-axis chosen");
			this.active_axis = 2;
			break;
		}
		this.active_rotation_axis = axis;
	}

	public void reInit()
	{
		for (int i=0;i<this.Limbs.length;i++)
			Limbs[i].reInit();

		this.body_rotation = 0;
	}

	/**
	 * Toggles the specified body part to be affected by rotations.
	 * 
	 * @param bodyPart
	 *          The bodyPart to toggle.
	 */
	public void toggle_part(BodyPartType bodyPart) {
		int loc = 0;

		switch (bodyPart) {
		case REAR_LEFT_LEG:
			loc = 0;
			System.out.println(" Toggling rear left Leg");
			break;
		case REAR_RIGHT_LEG:
			loc = 1;
			System.out.println(" Toggling rear right Leg");
			break;
		case FRONT_LEFT_LEG:
			loc = 2;
			System.out.println(" Toggling front left Leg");
			break;
		case FRONT_RIGHT_LEG:
			loc = 3;
			System.out.println(" Toggling front right Leg");
			break;
		case HEAD:
			loc = 4;
			System.out.println(" Toggling head");
			break;
		case TAIL:
			loc = 5;
			System.out.println(" Toggling tail");
			break;
		case NECK:
			loc = 6;
			System.out.println(" Toggling neck");
			break;
		}
		this.active_limb = loc;
		this.active_parts[loc] = !this.active_parts[loc];
	}

	/**
	 * Updates the current model of the dog.
	 * 
	 * @param gl
	 *          The OpenGL object with which to draw the dog.
	 */
	public void update(final GL gl) {
		int leg_list = this.dog_positions[0]+7;
		int body = leg_list+1;
		int all_legs = body+1;
		int head = all_legs+1;
		int tail = head+1;

		int l;



		//The update condition, which allows the display list to be updated depending on user input (allowing us to control the limbs)
		if (this.state_has_changed) 
		{
			gl.glNewList(this.dog_positions[0], GL.GL_COMPILE);
			{

				gl.glTranslatef(0, 1f, 0);
				gl.glCallList(body);
				gl.glCallList(all_legs);
				gl.glCallList(head);
				gl.glCallList(tail);
			}
			gl.glEndList();

			//Body
			gl.glNewList(body, GL.GL_COMPILE);
			{
				gl.glPushMatrix();
				{
					gl.glColor3f(.54f, .8f, .20f);
					gl.glTranslatef(0, -.5f, 0);
					gl.glRotatef(this.body_rotation, 0, 0, 1);
					gl.glScalef(2f, 1, 1.5f);
					this.glut.glutSolidSphere(.75, 36, 18);
				}
				gl.glPopMatrix();
			}
			gl.glEndList();

			//Display List that draws all four Limbs depending on the information relevant to that Limb
			gl.glNewList(all_legs, GL.GL_COMPILE);
			{

				for (l=0;l<4;l++)
				{
					gl.glPushMatrix();
					{
						gl.glTranslatef(Limbs[l].p0[0], Limbs[l].p0[1], Limbs[l].p0[2]);
						gl.glRotatef((float)Limbs[l].body_angle, 1, 0, 0);
						{
							gl.glRotatef(Limbs[l].upper_angle[0], 1, 0, 0);
							gl.glRotatef(Limbs[l].upper_angle[1], 0, 1, 0);
							gl.glRotatef(Limbs[l].upper_angle[2], 0, 0, 1);
						}
						gl.glCallList(leg_list);
						if (Limbs[l].p0[2]<0)
							Limbs[1].body_angle = -Limbs[1].body_angle;
						gl.glRotatef(-Limbs[l].body_angle, 1, 0, 0);
						{
							gl.glRotatef(Limbs[l].lower_angle[0], 1, 0, 0);
							gl.glRotatef(Limbs[l].lower_angle[1], 0, 1, 0);
							gl.glRotatef(Limbs[l].lower_angle[2], 0, 0, 1);
						}
						//gl.glCallList(rotate_lower);
						gl.glCallList(leg_list);
						gl.glRotatef(90, -1, 0, 0);
						{
							gl.glRotatef(Limbs[l].paw_angle[0], 1, 0, 0);
							gl.glRotatef(Limbs[l].paw_angle[1], 0, 1, 0);
							gl.glRotatef(Limbs[l].paw_angle[2], 0, 0, 1);
						}
						//gl.glCallList(rotate_paw);

						this.glut.glutSolidCone(.75f, .6f, 18, 9); 
						gl.glColor3f(.30f, .30f, .30f);
					}
					gl.glPopMatrix();
				}
			}
			gl.glEndList();

			//Draws the head and the neck, I could have made this into two display lists but it seemed unecessary
			gl.glNewList(head, GL.GL_COMPILE);
			{
				//Neck
				gl.glPushMatrix();
				{
					gl.glRotatef(30, 0, 0, 1);
					{
						gl.glRotatef(this.Limbs[6].upper_angle[0], 1, 0, 0);
						gl.glRotatef(this.Limbs[6].upper_angle[1], 0, 1, 0);
						gl.glRotatef(this.Limbs[6].upper_angle[2], 0, 0, 1);
					}
					gl.glTranslatef(1.25f, -.75f, 0);
					gl.glScalef(1f, .25f, .35f);
					this.glut.glutSolidSphere(.75, 36, 18);

					gl.glScalef(1f, 4f, 4f);
					gl.glTranslatef(this.Limbs[4].p0[0], 0, 0);
					gl.glRotatef(-30, 0, 0, 1);
					gl.glRotatef(90, 0, 1, 0);
					{
						gl.glRotatef(this.Limbs[4].upper_angle[0], 1, 0, 0);
						gl.glRotatef(this.Limbs[4].upper_angle[1], 0, 1, 0);
						gl.glRotatef(this.Limbs[4].upper_angle[2], 0, 0, 1);
					}
					gl.glScalef(1.25f, .75f, .5f);
					this.glut.glutSolidCylinder(.5, 1.5, 18, 9);
					gl.glTranslatef(0, 0, .25f);
					gl.glScalef(.5f,2f, .5f);
					gl.glColor3f(.54f, .8f, .20f);
					this.glut.glutSolidSphere(.5, 8, 4);
				}
				gl.glPopMatrix();
			}
			gl.glEndList();


			//Tail
			gl.glNewList(tail, GL.GL_COMPILE);
			{
				gl.glPushMatrix();
				{
					gl.glColor3f(.30f, .30f, .30f);
					{
						gl.glRotatef(this.Limbs[5].upper_angle[0], 1, 0, 0);
						gl.glRotatef(this.Limbs[5].upper_angle[1], 0, 1, 0);
						gl.glRotatef(this.Limbs[5].upper_angle[2], 0, 0, 1);
					}
					gl.glTranslatef(this.Limbs[5].p0[0], 0, 0);
					gl.glRotatef(-30, 0, 0, 1);

					gl.glScalef(1.25f, .15f, .15f);
					this.glut.glutSolidIcosahedron();
					gl.glTranslatef(-this.Limbs[5].p0[0], 0, 0);
				}
				gl.glPopMatrix();
			}
			gl.glEndList();

			//A list that is called twice for each leg. It draws the circular joint and rectangular appendage that each Limb has, making rotations easy to manage
			gl.glNewList(leg_list, GL.GL_COMPILE);
			{
				gl.glColor3f(.30f, .30f, .30f);
				this.glut.glutSolidSphere(.3, 36, 18);
				gl.glColor3f(.54f, .8f, .20f);
				gl.glTranslatef(0, -.75f, 0);
				gl.glScalef(.25f, 1.25f, .25f);
				this.glut.glutSolidCube(1);
				gl.glScalef(4f, .8f, 4f);
				gl.glTranslatef(0, -.6f, 0);
				gl.glColor3f(.30f, .30f, .30f);
			}
			gl.glEndList();

		}

		/*This massive block of code represents the display lists for the different positions the dog can be in for which the user has no control. These lists are called and 
		 * not again updated, creating more or less a still image of the dog that can be rotated and observed. 
		 */

		//The display list for the sitting dog
		gl.glNewList(this.dog_positions[1], GL.GL_COMPILE);
		{
			gl.glTranslatef(-1f, 0, 0);
			gl.glPushMatrix();
			{
				gl.glRotatef(30, 0, 0, 1);
				gl.glCallList(body);
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(-.75f, -1f, .5f);
				gl.glRotatef(-115, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(115, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(-.75f, -1f, -.5f);
				gl.glRotatef(115, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-115, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(1.25f, 0, -.5f);
				gl.glRotatef(45, 1, -1, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(90, 0, 0, 1);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();


			gl.glPushMatrix();
			{
				gl.glTranslatef(1.25f, 0, .5f);
				gl.glRotatef(-45, 1, -1, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(90, 0, 0, 1);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glCallList(head);
			}
			gl.glPopMatrix();
			gl.glTranslatef(1f, -.5f, 0);
			gl.glCallList(tail);
		}
		gl.glEndList();

		//The begging position
		gl.glNewList(this.dog_positions[2], GL.GL_COMPILE);
		{
			gl.glTranslatef(-1f, 0, 0);
			gl.glPushMatrix();
			{
				gl.glRotatef(55, 0, 0, 1);
				gl.glCallList(body);
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(-.5f, -1f, .5f);
				gl.glRotatef(-115, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(115, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(-.5f, -1f, -.5f);
				gl.glRotatef(115, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-115, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(1.25f, 0, -.5f);
				gl.glRotatef(105, 0, 0, 1);
				//gl.glRotatef(90, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 0, 0, 1);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();


			gl.glPushMatrix();
			{
				gl.glTranslatef(1.25f, 0, .5f);
				gl.glRotatef(105, 0, 0, 1);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 0, 0, 1);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(0, 1, 0);
				gl.glCallList(head);
			}
			gl.glPopMatrix();
			gl.glTranslatef(1, -.5f, 0);
			gl.glCallList(tail);
		}
		gl.glEndList();

		//The lay down position
		gl.glNewList(this.dog_positions[3], GL.GL_COMPILE);
		{
			gl.glPushMatrix();
			{
				gl.glTranslatef(0, -.25f, 0);
				gl.glCallList(body);
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(-.5f, -1f, 1f);
				gl.glRotatef(-45, 0, 1, 0);
				gl.glRotatef(-135, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(135, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(-.5f, -1f, -1f);
				gl.glRotatef(45, 0, 1, 0);
				gl.glRotatef(135, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-135, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(.5f, -1f, 1f);
				gl.glRotatef(45, 0, 1, 0);
				gl.glRotatef(-135, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(135, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();


			gl.glPushMatrix();
			{
				gl.glTranslatef(.5f, -1f, -1f);
				gl.glRotatef(-45, 0, 1, 0);
				gl.glRotatef(135, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-135, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(0, -.5f, 0);
				gl.glCallList(head);
			}
			gl.glPopMatrix();
			gl.glCallList(tail);
		}
		gl.glEndList();

		//The roll over position
		gl.glNewList(this.dog_positions[4], GL.GL_COMPILE);
		{
			gl.glPushMatrix();
			{
				gl.glTranslatef(0, -.25f, 0);
				gl.glCallList(body);
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(-.5f, -1f, 1f);
				gl.glRotatef(-45, 0, 1, 0);
				gl.glRotatef(-135, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-60, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(-.5f, -1f, -1f);
				gl.glRotatef(45, 0, 1, 0);
				gl.glRotatef(135, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(60, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(.5f, -1f, 1f);
				gl.glRotatef(45, 0, 1, 0);
				gl.glRotatef(-135, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-60, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();


			gl.glPushMatrix();
			{
				gl.glTranslatef(.5f, -1f, -1f);
				gl.glRotatef(-45, 0, 1, 0);
				gl.glRotatef(135, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(60, 1, 0, 0);
				gl.glCallList(leg_list);
				gl.glTranslatef(0, -.25f, 0);
				gl.glRotatef(-90, 1, 0, 0);
				this.glut.glutSolidCone(.75f, .6f, 18, 9); 
			}
			gl.glPopMatrix();

			gl.glPushMatrix();
			{
				gl.glTranslatef(.5f, -1.5f, 0);
				gl.glScalef(1, -1, 1);
				gl.glRotatef(-45, 0, 0, 1);
				gl.glCallList(head);
			}
			gl.glPopMatrix();
			gl.glCallList(tail);
		}
		gl.glEndList();

		//The jumping position, I wasn't sure if they wanted an animation though (this is just a still image, more or less)
		gl.glNewList(this.dog_positions[5], GL.GL_COMPILE);
		{
			if (this.current_position==5){
				this.Limbs[0].upper_angle[2]=-45;
				this.Limbs[1].upper_angle[2]=45;
				this.Limbs[2].upper_angle[2]=-45;
				this.Limbs[3].upper_angle[2]=45;
				this.body_rotation = 15;

				gl.glCallList(this.dog_positions[0]);
			}
		}
		gl.glEndList();

		//The display list for the dog giving paw (a high 5)
		gl.glNewList(this.dog_positions[6], GL.GL_COMPILE);
		if (this.current_position==6){
			{
				gl.glTranslatef(-1.5f, 0, 0);
				gl.glPushMatrix();
				{
					gl.glRotatef(55, 0, 0, 1);
					gl.glCallList(body);
				}
				gl.glPopMatrix();

				gl.glPushMatrix();
				{
					gl.glTranslatef(-.5f, -1f, .5f);
					gl.glRotatef(-115, 1, 0, 0);
					gl.glCallList(leg_list);
					gl.glTranslatef(0, -.25f, 0);
					gl.glRotatef(115, 1, 0, 0);
					gl.glCallList(leg_list);
					gl.glTranslatef(0, -.25f, 0);
					gl.glRotatef(-90, 1, 0, 0);
					this.glut.glutSolidCone(.75f, .6f, 18, 9); 
				}
				gl.glPopMatrix();

				gl.glPushMatrix();
				{
					gl.glTranslatef(-.5f, -1f, -.5f);
					gl.glRotatef(115, 1, 0, 0);
					gl.glCallList(leg_list);
					gl.glTranslatef(0, -.25f, 0);
					gl.glRotatef(-115, 1, 0, 0);
					gl.glCallList(leg_list);
					gl.glTranslatef(0, -.25f, 0);
					gl.glRotatef(-90, 1, 0, 0);
					this.glut.glutSolidCone(.75f, .6f, 18, 9); 
				}
				gl.glPopMatrix();

				gl.glPushMatrix();
				{
					gl.glTranslatef(1.25f, 0, -.5f);
					gl.glRotatef(60, 0, 0, 1);
					//gl.glRotatef(90, 1, 0, 0);
					gl.glCallList(leg_list);
					gl.glTranslatef(0, -.25f, 0);
					gl.glRotatef(-90, 0, 0, 1);
					gl.glCallList(leg_list);
					gl.glTranslatef(0, -.25f, 0);
					gl.glRotatef(-90, 1, 0, 0);
					this.glut.glutSolidCone(.75f, .6f, 18, 9); 
				}
				gl.glPopMatrix();


				gl.glPushMatrix();
				{
					gl.glTranslatef(1.25f, 0, .5f);
					gl.glRotatef(105, 0, 0, 1);
					gl.glCallList(leg_list);
					gl.glTranslatef(0, -.25f, 0);
					gl.glRotatef(-40, 0, 0, 1);
					gl.glCallList(leg_list);
					gl.glTranslatef(-.3f, 0, 0);
					gl.glRotatef(-90, 1, 0, 0);
					gl.glRotatef(60, 0, 1, 0);
					this.glut.glutSolidCone(.75f, .6f, 18, 9); 
				}
				gl.glPopMatrix();

				gl.glPushMatrix();
				{
					gl.glTranslatef(0, 1, 0);
					gl.glCallList(head);
				}
				gl.glPopMatrix();
				gl.glTranslatef(1, -.5f, 0);
				gl.glCallList(tail);
			}
		}
		gl.glEndList();

	}
}
