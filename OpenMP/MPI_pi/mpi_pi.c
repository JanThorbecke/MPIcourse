#include "mpi.h"
#include <math.h>
#include<stdio.h>
#include <time.h>
#include <sys/time.h>

double wallclock_time(void);

int main(int argc, char *argv[])
{	
	int n, myid, numprocs, i, rc;
	double PI25DT = 3.141592653589793238462643;
	double mypi, pi, h, sum, x, a;
	double t0, t1;

	MPI_Init(&argc,&argv);
	MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
	MPI_Comm_rank(MPI_COMM_WORLD,&myid);
	if (myid == 0) {
		n=1000000000;
		fprintf(stderr,"The number of intervals = %d\n", n);
		fprintf(stderr,"Running with %d MPI processes. \n", numprocs);
	}
	MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);
	h   = 1.0 / (double) n;
	sum = 0.0;
	t0=wallclock_time();
	for (i = myid + 1; i <= n; i += numprocs) {
		x = ((double)i - 0.5) * h;
		sum += 4.0 / (1.0 + x*x);
	}
	mypi = h * sum;	
	MPI_Reduce(&mypi, &pi, 1, MPI_DOUBLE, MPI_SUM, 0,MPI_COMM_WORLD);
	t1=wallclock_time();
	if (myid == 0) {
		printf("pi is approximately %.16f, Error is %.16f\n",pi, fabs(pi - PI25DT));
		fprintf(stderr,"walltime=%e\n",t1-t0);
	}
	MPI_Finalize();
	return 0;
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

