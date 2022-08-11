# Makefile

#MPI compiler 
MPICC	= mpicc
MPIFC	= mpif90
#MPICC	= gcc
MPICFLAGS 	= -O1 
MPIFFLAGS 	= -O1


all: my_exa1 my_exa2

my_exa1: my_exa1.c
	$(MPICC) $(MPICFLAGS) $@.c -o $@ -lm

my_exa2: my_exa2.c
	$(MPICC) $(MPICFLAGS) $@.c -o $@ -lm

.c.o: .c
	$(MPICC) $(CFLAGS) -c $<

.f90.o: .f90
	$(MPIF90) $(CFLAGS) -c $<

clean:
	rm -f core *.o a.out my_exa1 my_exa2

print:	Makefile $(SRC)
	$(PRINT) $?
	@touch print

count:
	@wc $(SRC)

