#include <time.h>
#include <sys/time.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

double wallclock_time();
#define NMAX 12

int main(int argc, char *argv[])
{
	float a[NMAX+1],  b[NMAX];
	int i;
	double t0, t1, t2;

	for (i=0; i<NMAX; i++) {
		a[i] = 1;
		b[i] = 2;
	}

	t0 = wallclock_time();
	#pragma omp parallel for shared(a,b)
	for (i=0; i<NMAX; i++) {
		a[i+1] = a[i]+b[i];
	}
	t1 = wallclock_time();
	fprintf(stderr,"shared cache loop = %e seconds\n", t1-t0);
	fprintf(stderr,"a=" );
	for (i=0; i<NMAX; i++) {
		fprintf(stderr,"%4.1f, ", a[i] );
	}
	fprintf(stderr,"\n" );

	return 0;
}

double wallclock_time()
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
                   (double)(1e-6*(s_val.tv_usec-b_val.tv_usec));

        return (double)time;
}

