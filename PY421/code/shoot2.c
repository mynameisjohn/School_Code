/* This program simulates the motion of a projectile under
   the combined effect of the gravitational and air drag
   and allows the user to modify the elevation until 
   the target is hit.

   Claudio Rebbi, Boston University, September 2001 */

#include <stdio.h>
#include <math.h>
#include <time.h>

main(){

  double g=9.8, drag=0.005, m=200, v0=400, dt=0.001, delta=0.05;
  double target,elev,theta,t,x,y,vx,vy,v,dx,dy,dvx,dvy,range,dist;
  int nshots;
  time_t tnow;

  /* incorporate some randomness in the target's distance: */

  time(&tnow);
  printf("%d\n",tnow);
  tnow=tnow%60;
  target=6+0.08*tnow;
  printf("\nThe target is at %6.2f Km.\n",target);
  printf("Shoot within 50m for a hit.\n\n");  

  dist=target;
  nshots=0;
  while(fabs(dist)>delta){

   printf("Enter the elevation (in degrees): ");
   scanf("%lf",&elev);
 
   theta=M_PI*elev/180;
 
   vx=v0*cos(theta);
   vy=v0*sin(theta);
 
   /* solve the equations of motion with the Euler  
      (or one-step) method: */
 
   t=0;
   x=0;
   y=0;
   while(y>=0){
     dx=vx*dt;
     dy=vy*dt;
     v=sqrt(vx*vx+vy*vy);
     dvx=-drag*v*vx*dt/m;
     dvy=-g*dt-drag*v*vy*dt/m;
     x+=dx;
     y+=dy;
     vx+=dvx;
     vy+=dvy;
     t+=dt;
   }
  
   /* correct for going below the x-axis */

   x=x-dx*y/dy;
   range=x/1000;  
   dist=target-range;
   nshots++;
   printf("Distance to target (negative for overshot): %7.2f\n",dist);
  
  }
  
  printf("\nTarget hit in %d shots!\n\n",nshots);

}
