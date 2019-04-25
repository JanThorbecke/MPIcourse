#include<stdio.h>
#include<math.h>
#include<omp.h>
#include <time.h>
#include <sys/time.h>

double wallclock_time(void);

static long num_steps=1000000; 
double step, pi;

int main()
{
	int i, num_procs, myid;	
	double x, sum = 0.0;
	double PI25DT = 3.141592653589793238462643;
	double t0, t1;

	num_procs = omp_get_num_procs();

	step = 1.0/(double) num_steps;
	fprintf(stderr,"The number of intervals = %ld\n", num_steps);
	fprintf(stderr,"Available are %d OpenMP threads. \n", num_procs);
	#pragma omp parallel
	{
	#pragma omp single
    fprintf(stderr,"Using %d threads in parallel region.\n", omp_get_num_threads( ));
	}

    t0=wallclock_time();
	#pragma omp parallel private(x) shared(sum)
{
	#pragma omp for 
	for (i=0; i<num_steps; i++){
		x = ((double)i+0.5)*step;
		sum = sum + 4.0/(1.0 + x*x);
	}
}
	pi = step * sum;
	t1=wallclock_time();
	fprintf(stderr,"pi is approximately %.16f, Error is %.16f\n",pi, fabs(pi - PI25DT));
	fprintf(stderr,"walltime=%e\n",t1-t0);
}



double wallclock_time(void)
{
	struct timeval s_val;
	static struct timeval b_val;
	double time;
	static int base=0;

	gettimeofday(&s_val,0);

	if (!base) {
		b_val = s_val;
		base = 1;
		return 0.0;
	}

	time = (double)(s_val.tv_sec-b_val.tv_sec) + 
		   (double)(1e-6*((double)s_val.tv_usec-(double)b_val.tv_usec));

	return (double)time;
}

//#pragma omp critical
