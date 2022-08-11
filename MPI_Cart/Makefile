# Makefile

#MPI compiler 
MPICC	= mpicc
MPIF90	= mpifort
#MPICC	= gcc
MPICFLAGS 	= -O3 
MPIFFLAGS 	= -O3

all: ring_2d_sr 

ring_2d_sr: ring_2d_sr.c
	$(MPICC) $(MPICFLAGS) $@.c -o $@ $(MPILIBS) -lm

jacobi: jacobi.f90
	$(MPIF90) $(MPIFFLAGS) $@.f90 -o $@ $(MPILIBS) -lm

.c.o: .c
	$(MPICC) $(CFLAGS) -c $<

clean:
	rm -f core *.o ring_2d_sr jacobi

print:	Makefile $(SRC)
	$(PRINT) $?
	@touch print

count:
	@wc $(SRC)

