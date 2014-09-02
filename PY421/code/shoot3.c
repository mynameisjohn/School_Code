/* This program simulates the motion of a projectile under
   the combined effect of the gravitational and air drag.
   It writes the results on the file "shoot.data" for
   subsequent plotting.

   Claudio Rebbi, Boston University, September 2001 */

#include <stdio.h>
#include <math.h>

main(){

  double g=9.8, drag=0.005, m=200, v0=400, dt=0.001;
  double elev,theta,t,x,y,vx,vy,v,dx,dy,dvx,dvy,range;
  FILE *data;
  double xo,dxo=10;  /* the data will be output only in intervals of ~dxo */
  double ynd; /* y without drag, for plotting */
  double vx0,vy0;

  printf("Enter the elevation (in degrees): ");
  scanf("%lf",&elev);

  theta=M_PI*elev/180;

  vx=v0*cos(theta);
  vy=v0*sin(theta);
  vx0=vx;
  vy0=vy;

  /* result for 0 drag: */

  t=2*vy/g;
  range=vx*t/1000;
  printf("\nRange for zero drag (in Km): %6.2f\n",range);

  /* solve the equations of motion with the Euler  
     (or one-step) method: */

  data=fopen("shoot.data","w");
  t=0;
  x=0;
  y=0;
  xo=dxo;
  while(y>=0){
    if(x>xo){
      ynd=vy0*x/vx0-g*x*x/(2*vx0*vx0);
      fprintf(data,"%f  %f  %f\n",x,y,ynd);
      xo+=dxo;
    }
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
  printf("\nRange(Km) and time (sec.):   %6.2f   %6.2f\n\n",range,t);

  ynd=vy0*x/vx0-g*x*x/(2*vx0*vx0);
  fprintf(data,"%f  %f  %f\n",x,y,ynd);
  fclose(data);

}