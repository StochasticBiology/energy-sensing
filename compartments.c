#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define RND drand48()

// produce gaussian random number
double gsl_ran_gaussian(const double sigma)
{
  double x, y, r2;

  do
    {
      /* choose x,y in uniform square (-1,-1) to (+1,+1) */

      x = -1 + 2 * RND;
      y = -1 + 2 * RND;

      /* see if it is in the unit circle */
      r2 = x * x + y * y;
    }
  while (r2 > 1.0 || r2 == 0);

  /* Box-Muller transform */
  return sigma * y * sqrt (-2.0 * log (r2) / r2);
}

int main(void)
{
  double I, A, S, F, dI, dA, dS, dF;
  double t, dt;
  double alpha;
  FILE *fp, *fp1;
  double rho, sigma, delta, epsilon;
  int period, positive;
  double kernel;
  int sample;

  // sim1 -- normal -- how do cost and effect of sensing affect performance in different environments
  // for each experiment we'll output some sample time behaviour and a summary in the "long-term" limit  
  fp = fopen("sim-out.csv", "w");
  fprintf(fp, "period,positive,delta,epsilon,t,alpha,I,A,S,F\n");

  fp1 = fopen("sim-out-sum.csv", "w");
  fprintf(fp1, "period,positive,delta,epsilon,S.ratio\n");

  // initial constants
  dt = 0.01;
  rho = sigma = 1;
  // period and positive determine the environmental time series, with period (-1 = linear, 0 = const) and initial gradient (+ve/-ve)
  for(period = -1; period < 10; period++)
    {
      for(positive = -1; positive <= 1; positive+=2)
	{
	  // delta and epsilon are cost and effect of sensing respectively
	  for(delta = 0; delta <= 1; delta += 0.1)
	    {
	      for(epsilon = 0; epsilon <= 1; epsilon += 0.1)
		{
		  // simulate the system
		  I = 1; A = S = F = 0;
		  for(t = 0; t < 20; t += dt)
		    {
		      // environmental time series
		      switch(period)
			{
			case 0: alpha = 0.5; break;
			case -1: alpha = 0.1*t; break;
			default: alpha = 0.5 + positive*0.5*sin(2*3.1416*t/period); break;
			}

		      // increments
		      dI = dt*( (rho - rho*epsilon*(alpha-0.5))*A - (sigma + sigma*epsilon*(alpha-0.5))*I );
		      dA = dt*( (sigma + sigma*epsilon*(alpha-0.5))*I - ((1.-delta)*alpha + (1.-(1.-delta)*alpha) + (rho - rho*epsilon*(alpha-0.5)))*A );
		      dS = dt*( (1.-delta)*alpha*A );
		      dF = dt*( (1.-(1.-delta)*alpha)*A );
		      I += dI; A += dA; S += dS; F += dF;

		      // output time series under set conditions
		      if(delta == 0 && (epsilon == 0 || epsilon > 0.99) && (int)((t+dt)*10) > (int)(t*10))
			fprintf(fp, "%i,%i,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e\n", period, positive, delta, epsilon, t, alpha, I, A, S, F);
		    }
		  // output summary for these params
		  fprintf(fp1, "%i,%i,%.3e,%.3e,%.3e\n", period, positive, delta, epsilon, S/(S+F));
		}
	    }
	}
    }
  fclose(fp);
  fclose(fp1);

  // sim2 -- vary rho and sigma, the propensity to "rest" vs "seek"
  // for each experiment we'll output some sample time behaviour and a summary in the "long-term" limit  
  fp = fopen("sim2-out.csv", "w");
  fprintf(fp, "period,positive,rho,sigma,epsilon,t,alpha,I,A,S,F\n");

  fp1 = fopen("sim2-out-sum.csv", "w");
  fprintf(fp1, "period,positive,rho,sigma,epsilon,S.ratio\n");

  // initial constants
  dt = 0.01;
  delta = 0;
  // period and positive determine the environmental time series, with period (-1 = linear, 0 = const) and initial gradient (+ve/-ve)
  for(period = -1; period < 10; period++)
    {
      for(positive = -1; positive <= 1; positive+=2)
	{
	  // rho is flux from seeking to resting; sigma is flux from resting to seeking
	  for(rho = 0.01; rho < 1; rho *= 2)
	    {
	      for(sigma = 0.01; sigma < 1; sigma *= 2)
		{
		  for(epsilon = 0; epsilon <= 1; epsilon++)
		    {
		      // simulate the system -- for longer times this time, we'll check post hoc for convergence
		      I = 1; A = S = F = 0;
		      for(t = 0; t < 200; t += dt)
			{
			  // environmental time series
			  switch(period)
			    {
			    case 0: alpha = 0.5; break;
			    case -1: alpha = 0.1*t; break;
			    default: alpha = 0.5 + positive*0.5*sin(2*3.1416*t/period); break;
			    }

			  // increments
			  dI = dt*( (rho - rho*epsilon*(alpha-0.5))*A - (sigma + sigma*epsilon*(alpha-0.5))*I );
			  dA = dt*( (sigma + sigma*epsilon*(alpha-0.5))*I - ((1.-delta)*alpha + (1.-(1.-delta)*alpha) + (rho - rho*epsilon*(alpha-0.5)))*A );
			  dS = dt*( (1.-delta)*alpha*A );
			  dF = dt*( (1.-(1.-delta)*alpha)*A );
			  I += dI; A += dA; S += dS; F += dF;

			  // output time series under set conditions
			  if(rho > 0.6 && sigma == 0.01 && (int)((t+dt)*10) > (int)(t*10))
			    fprintf(fp, "%i,%i,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e\n", period, positive, rho, sigma, epsilon, t, alpha, I, A, S, F);

			}
		      // output summary for these params
		      fprintf(fp1, "%i,%i,%.3e,%.3e,%.3e,%.3e\n", period, positive, rho, sigma, epsilon, S/(S+F));
		    }
		}
	    }
	}
    }
  fclose(fp);
  fclose(fp1);

  // sim3 -- same as sim1, but with different rho and sigma for sanity
  // for each experiment we'll output some sample time behaviour and a summary in the "long-term" limit  
  fp = fopen("sim3-out.csv", "w");
  fprintf(fp, "period,positive,delta,epsilon,t,alpha,I,A,S,F\n");

  fp1 = fopen("sim3-out-sum.csv", "w");
  fprintf(fp1, "period,positive,delta,epsilon,S.ratio\n");

  // initial constants -- changed relative to sim1
  dt = 0.01;
  rho = 0.1; sigma = 0.6;
  // period and positive determine the environmental time series, with period (-1 = linear, 0 = const) and initial gradient (+ve/-ve)
  for(period = -1; period < 10; period++)
    {
      for(positive = -1; positive <= 1; positive+=2)
	{
	  // delta and epsilon are cost and effect of sensing respectively
	  for(delta = 0; delta <= 1; delta += 0.1)
	    {
	      for(epsilon = 0; epsilon <= 1; epsilon += 0.1)
		{
		  // simulate the system -- for longer times this time, we'll check post hoc for convergence
		  I = 1; A = S = F = 0;
		  for(t = 0; t < 200; t += dt)
		    {
		      // environmental time series
		      switch(period)
			{
			case 0: alpha = 0.5; break;
			case -1: alpha = 0.1*t; break;
			default: alpha = 0.5 + positive*0.5*sin(2*3.1416*t/period); break;
			}

		      // increments
		      dI = dt*( (rho - rho*epsilon*(alpha-0.5))*A - (sigma + sigma*epsilon*(alpha-0.5))*I );
		      dA = dt*( (sigma + sigma*epsilon*(alpha-0.5))*I - ((1.-delta)*alpha + (1.-(1.-delta)*alpha) + (rho - rho*epsilon*(alpha-0.5)))*A );
		      dS = dt*( (1.-delta)*alpha*A );
		      dF = dt*( (1.-(1.-delta)*alpha)*A );
		      I += dI; A += dA; S += dS; F += dF;

		      // output time series under set conditions
		      if(delta == 0 && (epsilon == 0 || epsilon > 0.99) && (int)((t+dt)*10) > (int)(t*10))
			fprintf(fp, "%i,%i,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e\n", period, positive, delta, epsilon, t, alpha, I, A, S, F);
		    }
		  // output summary for these params
		  fprintf(fp1, "%i,%i,%.3e,%.3e,%.3e\n", period, positive, delta, epsilon, S/(S+F));
		}
	    }
	}
    }
  fclose(fp);
  fclose(fp1);

  // sim4 -- stochastic environment with different rates of variance increase
  // for each experiment we'll output some sample time behaviour and a summary in the "long-term" limit  
  fp = fopen("sim4-out.csv", "w");
  fprintf(fp, "kernel,sample,delta,epsilon,t,alpha,I,A,S,F\n");

  fp1 = fopen("sim4-out-sum.csv", "w");
  fprintf(fp1, "kernel,sample,delta,epsilon,S.ratio\n");

  // initial constants 
  dt = 0.01;
  rho = sigma = 1;
  // kernel is the width of the perturbation kernel applied at each small timestep (dt) to the current environment
  for(kernel = 0.001; kernel < 1; kernel *= 10)
    {
      // loop over 100 samples of the environmental signal
      for(sample = 0; sample < 100; sample++)
	{
	  // delta and epsilon are cost and effect of sensing respectively
	  for(delta = 0; delta <= 1; delta += 0.1)
	    {
	      for(epsilon = 0; epsilon <= 1; epsilon += 0.1)
		{
		  // simulate the system 
		  I = 1; A = S = F = 0;
		  alpha = 0.5;
		  for(t = 0; t < 20; t += dt)
		    {
		      // apply random perturbation to environmental time series and truncate if needed
		      alpha += gsl_ran_gaussian(kernel);
		      if(alpha < 0) alpha = 0;
		      if(alpha > 1) alpha = 1;

		      // increments
		      dI = dt*( (rho - rho*epsilon*(alpha-0.5))*A - (sigma + sigma*epsilon*(alpha-0.5))*I );
		      dA = dt*( (sigma + sigma*epsilon*(alpha-0.5))*I - ((1.-delta)*alpha + (1.-(1.-delta)*alpha) + (rho - rho*epsilon*(alpha-0.5)))*A );
		      dS = dt*( (1.-delta)*alpha*A );
		      dF = dt*( (1.-(1.-delta)*alpha)*A );
		      I += dI; A += dA; S += dS; F += dF;

		      // output time series under set conditions
		      if(delta == 0 && (epsilon == 0 || epsilon > 0.99) && (int)((t+dt)*10) > (int)(t*10))
			fprintf(fp, "%.3e,%i,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e,%.3e\n", kernel, sample, delta, epsilon, t, alpha, I, A, S, F);
		    }
		  // output summary for these params
		  fprintf(fp1, "%.3e,%i,%.3e,%.3e,%.3e\n", kernel, sample, delta, epsilon, S/(S+F));
		}
	    }
	}
    }
  fclose(fp);
  fclose(fp1);

  return 0;
}
      
