# Makefile

#Intel compiler which supports openMP
CC	= icc
CFLAGS 	= -O3 -qopenmp -static

#GNU compiler which supports openMP
#CC	= gcc
#CFLAGS 	= -O3 -fopenmp

#MPI compiler 
MPICC	= mpiicc
MPIF90	= mpiif90
#MPICC	= gcc
#MPICFLAGS 	= -O3 -mpe=mpilog
#MPICFLAGS 	= -O3 -I/opt/mpich2/include  -m32
#MPICFLAGS 	= -O3  -static
#MPIFFLAGS 	= -O3  -static

#MPILIBS=-L/opt/mpich2/lib -lfmpich /u/thorbec/src/mpe2-1.0.6p1/lib/liblmpe.a /u/thorbec/src/mpe2-1.0.6p1/lib/libmpe.a -lmpich -lpthread -lrt
#MPILIBS=-L/u/thorbec/src/fpmpi-2.1f -lfpmpi

all: openmp_pi openmp_pi2 mpi_pi mpif_pi

openmp_pi: openmp_pi.c
	$(CC) $(CFLAGS) -o $@ $@.c $(LIBS) -lm

openmp_pi2: openmp_pi2.c
	$(CC) $(CFLAGS) -o $@ $@.c $(LIBS) -lm

mpi_pi: mpi_pi.c
	$(MPICC) $(MPICFLAGS) $@.c -o $@ $(MPILIBS) -lm

mpif_pi: mpif_pi.f90
	$(MPIF90) $(MPIFFLAGS) $@.f90 -o $@ $(MPILIBS) -lm

.c.o: .c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f core mpi_pi openmp_pi mpif_pi

print:	Makefile $(SRC)
	$(PRINT) $?
	@touch print

count:
	@wc $(SRC)

tar:
	@tar cf $(PRG).tar Makefile $(SRC) && compress $(PRG).tar
