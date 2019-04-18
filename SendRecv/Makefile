MPICC?=mpicc
MPIFC?=mpif90

all: c f90

c: basic.c
	${MPICC} -o basic_c basic.c

f90: basic.f90
	${MPIFC} -o basic_f90 basic.f90

clean:
	rm -f basic_c basic_f90
