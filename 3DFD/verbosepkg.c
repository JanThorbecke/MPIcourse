#include <stdio.h>
#include <stdarg.h>
#include "par.h"
#include <string.h>
#include <mpi.h>

/**
*  functions to print out verbose, error and warning messages to stderr.
*
*   AUTHOR:
*           Jan Thorbecke (janth@xs4all.nl)
*           The Netherlands 
**/

int my_pe;

void vinit()
{
	MPI_Comm_rank(MPI_COMM_WORLD, &my_pe);
	return;
}



void verr(char *fmt, ...)
{
	va_list args;

	if (my_pe==0) {
	if (EOF == fflush(stderr)) {
		fprintf(stderr, "\nverr: fflush failed on stderr");
	}
	fprintf(stderr, "    Error in %s: ", xargv[0]);
	va_start(args,fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	}
        MPI_Finalize();
	exit(EXIT_FAILURE);
}

void vwarn(char *fmt, ...)
{
	va_list args;

	if (my_pe==0) {
	if (EOF == fflush(stderr)) {
		fprintf(stderr, "\nvwarn: fflush failed on stderr");
	}
	fprintf(stderr, "    Warning in %s: ", xargv[0]);
	va_start(args,fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	}
	return;
}

void vmess(char *fmt, ...)
{
	va_list args;

	if (my_pe==0) {
	if (EOF == fflush(stderr)) {
		fprintf(stderr, "\nvmess: fflush failed on stderr");
	}
	fprintf(stderr, "    %s: ", xargv[0]);
	va_start(args,fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	}
	return;
}
