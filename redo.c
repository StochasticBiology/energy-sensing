#include <stdio.h>
#include <math.h>

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

int main(void)
{
  double beta, omega, phi;  // environment and cost
  double k0, kp, ki, kd;    // sensing parameters
  double stateI, stateA, stateW, stateL;  // state of system
  double dstateIdt, dstateAdt, dstateWdt, dstateLdt;   // d/dt
  double rho, sigma, kappa1, kappa2, delta;   // rate consts and associated quantities
  double env;
  double diff, prevdiff, ddiffdt, intdiffdt;  // used in PID calcs
  double t, dt = 0.01;
  FILE *fp;

  // test case outputting a time series
  if(0) {
    beta = 0; omega = 1; phi = 2.5; k0 = 0.25, kp = 0.75, ki = 0.75, kd = 1;
  
    stateI = 1; stateA = stateW = stateL = 0;
    //delta = 0;
    delta = beta*(k0 + kp + ki + kd);
    for(t = 0; t < 100; t += dt)
      {
	env = envfn(t, omega, phi);
	diff = env-0.5;
	ddiffdt = (t == 0 ? 0 : (diff-prevdiff)/dt);
	intdiffdt = (t == 0 ? 0 : intdiffdt + diff*dt);
	sigma = mymax(0, k0 + kp*diff + ki*intdiffdt + kd*ddiffdt);
	rho = mymax(0, k0 - kp*diff - ki*intdiffdt - kd*ddiffdt);
	kappa1 = mymax(0, env*(1.-delta));
	kappa2 = mymax(0, 1.-env*(1.-delta));
	//delta = delta + beta*(k0 + kp + ki + kd)*dt;
	dstateIdt = dt* ( -stateI*sigma + stateA*rho );
	dstateAdt = dt* (  stateI*sigma - stateA*rho - stateA*(kappa1+kappa2) );
	dstateWdt = dt* (  stateA*kappa1 );
	dstateLdt = dt* (  stateA*kappa2 );
	stateI += dstateIdt;
	stateA += dstateAdt;
	stateW += dstateWdt;
	stateL += dstateLdt;
	prevdiff = diff;
	printf("%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f\n", t, stateI, stateA, stateW, stateL, sigma, rho, kappa1, kappa2);
      }
    printf("%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f\n", beta, omega, phi, k0, kp, ki, kd, stateW, stateL, delta);

    return 0;
  }

  fp = fopen("redo-out.csv", "w");
  fprintf(fp, "beta,omega,phi,k0,kp,ki,kd,W,L,delta\n");
  // loop through sensing cost
  for(beta = 0; beta < 0.2; beta += 0.05)
    {
      // loop through environmental frequency
      for(omega = 0; omega < 5; omega ++)
	{
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
			      // initialise state
			      stateI = 1; stateA = stateW = stateL = 0;
			      //delta = 0;
			      // fix a delta value for this parameterisation
			      delta = beta*(k0 + kp + ki + kd);
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
				}
			      fprintf(fp, "%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f\n", beta, omega, phi, k0, kp, ki, kd, stateW, stateL, delta, t);
			    }
			}
		    }
		}
	    }
	  printf("%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f,%.3f\n", beta, omega, phi, k0, kp, ki, kd, stateW, stateL, delta);
	}
    }
  fclose(fp);
  
  return 0;
}
						     
