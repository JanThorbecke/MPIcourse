#module purge
#module load craype-haswell
#module load PrgEnv-cray
#module load cudatoolkit/7.0.28
#module load perftools

CC=mpicc
CFLAGS=-O3 
LIBS= -lm -lc

all: 3dfd

SRCC	= 3dfd.c \
	getpars.c \
	atopkge.c \
	docpkge.c \
	wallclock_time.c

OBJC	= $(SRCC:%.c=%.o)

.SUFFIXES : .o .c 

.c.o:
	$(CC) $(CFLAGS) -I. -c $<


3dfd:	$(OBJC) 
	$(CC) $(LDFLAGS) $(CFLAGS) $(OPTC) -o $@ $(OBJC) $(LIBS)

install: $(PRG) 
	cp $(PRG) $B

clean:
		rm -f core $(OBJC) 

realclean:
		rm -f core $(OBJC) $(PRG) 


print:	Makefile $(SRC)
	$(PRINT) $?
	@touch print

count:
	@wc $(SRC)

tar:
	@tar cf $(PRG).tar Makefile $(SRC) && compress $(PRG).tar



