#include <stdio.h>
#include <math.h>

#define _RK

#define PI 3.14159

// environmental function (nutrient available) non-negative sine wave
double envfn(double t, double omega, double phi)
{
  if(omega == 0) return 0.5;
  return 0.5+0.5*sin(omega*t + phi);
}

// return max of two values (used for thresholding a zero)
double mymax(double x, double y)
{
  if(x > y) return x;
  return y;
}

// simulate an instance of the system
void Simulate(double k0, double kp, double ki, double kd, double beta, double omega, double phi, double *W, double *L, double *rdelta, double *tend, int output, char *fname)
{
  double stateI, stateA, stateW, stateL;  // state of system
  double dstateIdt, dstateAdt, dstateWdt, dstateLdt;   // d/dt
  double rho, sigma, kappa1, kappa2, delta;   // rate consts and associated quantities
  double t, dt = 0.01;
  FILE *fp;
  double env;
  double diff, prevdiff, ddiffdt, intdiffdt;  // used in PID calcs
 
  if(output)
    {
      fp = fopen(fname, "w");
      fprintf(fp, "t, env, stateI, stateA, stateW, stateL, sigma, rho, kappa1, kappa2\n");
    }
      
  // initialise state
  stateI = 1; stateA = stateW = stateL = 0;
  //delta = 0;
  // fix a delta value for this parameterisation
  #ifdef _IGJ
  delta = beta*(k0 + kp + ki + kd);
  #endif
  // euler time simulation
  for(t = 0; t < 100 && (stateI + stateA) > 1e-6; t += dt)
    {
      // current environments
      env = envfn(t, omega, phi);
      // environment statistics for PID
      diff = env-0.5;
      ddiffdt = (t == 0 ? 0 : (diff-prevdiff)/dt);
      intdiffdt = (t == 0 ? 0 : intdiffdt + diff*dt);

      // rate constants
      sigma = mymax(0, k0 + kp*diff + ki*intdiffdt + kd*ddiffdt);
      rho = mymax(0, k0 - kp*diff - ki*intdiffdt - kd*ddiffdt);
      #ifdef _RK
      delta = mymax(0, beta*(kp*diff + ki*intdiffdt + kd*ddiffdt));
      #endif
      kappa1 = mymax(0, env*(1.-delta));
      kappa2 = mymax(0, 1.-env*(1.-delta));
      //delta = delta + beta*(k0 + kp + ki + kd)*dt;

      // time derivatives
      dstateIdt = dt* ( -stateI*sigma + stateA*rho );
      dstateAdt = dt* (  stateI*sigma - stateA*rho - stateA*(kappa1+kappa2) );
      dstateWdt = dt* (  stateA*kappa1 );
      dstateLdt = dt* (  stateA*kappa2 );

      // update state and environment statistic
      stateI += dstateIdt;
      stateA += dstateAdt;
      stateW += dstateWdt;
      stateL += dstateLdt;
      prevdiff = diff;

      if(output)
	{
	  fprintf(fp, "%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f\n", t, env, stateI, stateA, stateW, stateL, sigma, rho, kappa1, kappa2);
	}

    }
  *W = stateW; *L = stateL; *rdelta = delta; *tend = t;
}

int main(void)
{
  double beta, omega, phi;  // environment and cost
  double k0, kp, ki, kd;    // sensing parameters
  double tomega;
  double stateL, stateW, delta, tend;
  FILE *fp;
  double testomega = 1;

  //testomega = 2*PI;
  
  // collections of test cases outputting time series
  beta = 0; omega = 0; phi = 2.5; k0 = 1, kp = 0; ki = 0.0; kd = 0;
  Simulate(k0, kp, ki, kd, beta, omega, phi, &stateW, &stateL, &delta, &tend, 1, "example-0a.csv");
  beta = 0; omega = testomega; phi = 2.5; k0 = 1, kp = 0; ki = 0.0; kd = 0;
  Simulate(k0, kp, ki, kd, beta, omega, phi, &stateW, &stateL, &delta, &tend, 1, "example-0b.csv");
  beta = 0; omega = testomega; phi = 0; k0 = 1, kp = 0; ki = 0.0; kd = 0;
  Simulate(k0, kp, ki, kd, beta, omega, phi, &stateW, &stateL, &delta, &tend, 1, "example-0c.csv");
  beta = 0; omega = testomega; phi = 2.5; k0 = 0.25; kp = 0.75; ki = 0.75; kd = 1;
  Simulate(k0, kp, ki, kd, beta, omega, phi, &stateW, &stateL, &delta, &tend, 1, "example-1.csv");
  beta = 10; omega = testomega; phi = 2.5; k0 = 0.25; kp = 0.75; ki = 0.75; kd = 1;
  Simulate(k0, kp, ki, kd, beta, omega, phi, &stateW, &stateL, &delta, &tend, 1, "example-2.csv");

  //  return 0;
  #ifdef _RK
    fp = fopen("redo-out-rk.csv", "w");
  #else
    fp = fopen("redo-out.csv", "w");
  #endif
  fprintf(fp, "beta,omega,phi,k0,kp,ki,kd,W,L,delta,tend\n");
  // loop through sensing cost
  for(beta = 0; beta < 0.2; beta += 0.05)
    {
      // loop through environmental frequency
      for(tomega = 0.02; tomega < 5; tomega *= 5)
	{
	  if(tomega == 0.02) omega = 0; else omega = tomega;
	  // loop through environmental phase
	  for(phi = 0; phi < 2*PI; phi += 0.5)
	    {
	      // loop through PID terms
	      for(k0 = 0; k0 <= 1; k0 += 0.25)
		{
		  for(kp = 0; kp <= 1; kp += 0.25)
		    {
		      for(ki = 0; ki <= 1; ki += 0.25)
			{
			  for(kd = 0; kd <= 1; kd += 0.25)
			    {
			      Simulate(k0, kp, ki, kd, beta, omega, phi, &stateW, &stateL, &delta, &tend, 0, "tmp");
			      fprintf(fp, "%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f\n", beta, tomega, phi, k0, kp, ki, kd, stateW, stateL, delta, tend);
			    }
			}
		    }
		}
	    }
	  printf("%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f\n", beta, tomega, phi, k0, kp, ki, kd, stateW, stateL, delta);
	}
    }
  fclose(fp);
  
  return 0;
}
						     
