# Makefile

#MPI compiler 
MPICC	= mpicc
MPIFC	= mpif90
#MPICC	= gcc
MPICFLAGS 	= -O3 
MPIFFLAGS 	= -O3


all: userdefined_types1 userdefined_types2 userdefined_types3

userdefined_types1: userdefined_types1.f90
	$(MPIFC) $(MPIFFLAGS) $@.f90 -o $@ $(MPILIBS) -lm

userdefined_types2: userdefined_types2.f90
	$(MPIFC) $(MPIFFLAGS) $@.f90 -o $@ $(MPILIBS) -lm

userdefined_types3: userdefined_types3.f90
	$(MPIFC) $(MPIFFLAGS) $@.f90 -o $@ $(MPILIBS) -lm

userdefined_types1_sol: userdefined_types1_sol.f90
	$(MPIFC) $(MPIFFLAGS) $@.f90 -o $@ $(MPILIBS) -lm

userdefined_types3_sol: userdefined_types3_sol.f90
	$(MPIFC) $(MPIFFLAGS) $@.f90 -o $@ $(MPILIBS) -lm

.c.o: .c
	$(MPICC) $(CFLAGS) -c $<

.f90.o: .f90
	$(MPIF90) $(CFLAGS) -c $<

clean:
	rm -f core *.o userdefined_types1 userdefined_types2 userdefined_types3 a.out userdefined_types1_sol userdefined_types3_sol

print:	Makefile $(SRC)
	$(PRINT) $?
	@touch print

count:
	@wc $(SRC)

