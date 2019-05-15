#module purge
#module load craype-haswell
#module load PrgEnv-cray
#module load cudatoolkit/7.0.28
#module load perftools

CC=cc
CFLAGS=-O3 
LIBS=-L. -lgenfft -lm -lc

all: libgenfft 3dfd

PRG = 3dfd

SRCC	= $(PRG).c \
	getParameters.c \
	getModelInfo.c  \
	getWaveletInfo.c  \
	getWaveletHeaders.c  \
	readModel.c  \
	wallclock_time.c  \
	verbosepkg.c  \
	name_ext.c  \
	atopkge.c \
	docpkge.c \
	getpars.c



libgenfft:
	cd FFTlib		; $(MAKE)

OBJC	= $(SRCC:%.c=%.o)

.SUFFIXES : .o .c 

.c.o:
	$(CC) $(CFLAGS) -I. -c $<


$(PRG):	$(OBJC) fdelmodc.h
	$(CC) $(LDFLAGS) $(CFLAGS) $(OPTC) -o $(PRG) $(OBJC) $(LIBS)

install: $(PRG) 
	cp $(PRG) $B

clean:
		rm -f core $(OBJC) $(OBJM) $(PRG) 

realclean:
		rm -f core $(OBJC) $(OBJM) $(PRG) $B/$(PRG) 


print:	Makefile $(SRC)
	$(PRINT) $?
	@touch print

count:
	@wc $(SRC)

tar:
	@tar cf $(PRG).tar Makefile $(SRC) && compress $(PRG).tar



