#include <time.h>
#include <sys/time.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

double wallclock_time();
/* 32K L1 cache in core 2 duo */
#define NMAX 8192
//#define NMAX 2048
/* 6MB L2 cache in core 2 duo */
//#define NMAX 1572864
#define PS 32

int main()
{
// 	float a[NMAX], pad1[PS], b[NMAX], pad2[PS], c[NMAX], pad3[PS], d[NMAX];
	float a[NMAX],  b[NMAX],  c[NMAX],  d[NMAX];
//	float *a, *b, *c, *d, *p;
	int i;
	double t0, t1, t2;

/*
	p = (float *)malloc(4*(NMAX+PS)*sizeof(float));
	assert(p!=NULL);
	a = (float *)&p[0*(NMAX+PS)];
	b = (float *)&p[1*(NMAX+PS)];
	c = (float *)&p[2*(NMAX+PS)];
	d = (float *)&p[3*(NMAX+PS)];
*/

	for (i=0; i<NMAX; i++) {
		a[i] = 0.0;
		b[i] = i;
		c[i] = 2*i;
		d[i] = -0.5*i;
	}

	t0 = wallclock_time();
	#pragma ivdep
	for (i=0; i<NMAX; i++) {
		a[i] = b[i]+c[i]*d[i];
	}
	t1 = wallclock_time();
	fprintf(stderr,"one loop = %e seconds %f\n", t1-t0, a[NMAX/2]);

	for (i=0; i<NMAX; i++) {
		a[i] = 0.0;
		b[i] = i;
		c[i] = 2*i;
		d[i] = -0.5*i;
	}

	t0 = wallclock_time();
	for (i=0; i<NMAX; i++) {
		a[i] = d[i]*c[i];
	}
	for (i=0; i<(NMAX/sizeof(float)); i++) {
		a[i] = b[i]+a[i];
	}
	t1 = wallclock_time();
	fprintf(stderr,"two loops = %e seconds\n", t1-t0);

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

