void solveRK(T *x0, T I, T a, T b, T c, T d)
{
  T k1,k2,k3,k4,s;
  T j1,j2,j3,j4;
  float V, u;

  k1 = solveForVdot(x0[0],x0[1],I);
  j1 = solveForUdot(x0[0],x0[1],a,b);

  V = eulerAdvance(x0[0],k1,DT/2);
  u = eulerAdvance(x0[1],j1,DT/2);
  k2 = solveForVdot(V,u,I);
  j2 = solveForUdot(V,u,a,b);

  V = eulerAdvance(x0[0],k2,DT/2);
  u = eulerAdvance(x0[1],j2,DT/2);
  k3 = solveForVdot(V,u,I);
  j3 = solveForUdot(V,u,a,b);

  V = eulerAdvance(x0[0],k3,DT);
  u = eulerAdvance(x0[1],j3,DT);
  k4 = solveForVdot(V,u,I);
  j4 = solveForUdot(V,u,a,b);

  s = (k1+2*k2+2*k3+k4) / 6.0;
  V = eulerAdvance(x0[0], s, DT);

  if (V<V_SPIKE)
    {
      x0[0] = V; 
      s = (j1+2*j2+j2*j3+j4) / 6.0;
      x0[1] = eulerAdvance(x0[1], s, DT);
    }
  else
    {
      x0[0]=c;
      x0[1]=x0[1]+d;
    }
}

T solveForVdot(T V0, T u0, T I)
{// Equation as outlined by Izhkevich in the Assignment
  return 0.04*V0*V0+5*V0+140-u0+I;
}

T solveForUdot(T V0, T u0, T a, T b)
{// Equation as outlined by Izhkevich in the Assignment
  return a*(b*V0-u0);
}

T eulerAdvance(T x0, T v, T dt)
{
  return x0 + dt * v;
}
