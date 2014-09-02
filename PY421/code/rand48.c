double rand48_(x)
/*                    Function that wraps the C utility function
                      erand48 into a C function that makes it callable
                      from a fortran program.  This code should be
                      compiled in C and the resulting object should
                      be linked with the fortran program that uses
                      rand48.  In the fortran program the random
                      number generator should be called as follows

                      DOUBLE PRECISION rand48
                      INTEGER*2 seed(3)

                      random_number=rand48(seed)

                      See the manual entry under erand48 for further
                      clarifications.


                                       Claudio Rebbi - Boston University
                                                       April 1992 

*/

unsigned short x[3];
{
  double erand48();
  return erand48(x);
}

