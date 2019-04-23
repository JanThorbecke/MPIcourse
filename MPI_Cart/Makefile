# Makefile

#MPI compiler 
MPICC	= mpiicc
MPIF90	= mpiifort
#MPICC	= gcc
MPICFLAGS 	= -O3 
MPIFFLAGS 	= -O3

#MPILIBS=-L/opt/mpich2/lib -lfmpich /u/thorbec/src/mpe2-1.0.6p1/lib/liblmpe.a /u/thorbec/src/mpe2-1.0.6p1/lib/libmpe.a -lmpich -lpthread -lrt
#MPILIBS=-L/u/thorbec/src/fpmpi-2.1f -lfpmpi

all: ring_2d_sr 

ring_2d_sr: ring_2d_sr.c
	$(MPICC) $(MPICFLAGS) $@.c -o $@ $(MPILIBS) -lm

.c.o: .c
	$(MPICC) $(CFLAGS) -c $<

clean:
	rm -f core *.o ring_2d_sr

print:	Makefile $(SRC)
	$(PRINT) $?
	@touch print

count:
	@wc $(SRC)

