EXECS=groups split split2
MPICC?=mpicc

all: ${EXECS}

split: split.c
	${MPICC} -o split split.c

split2: split2.c
	${MPICC} -o split2 split2.c

groups: groups.c
	${MPICC} -o groups groups.c

clean:
	rm -f ${EXECS}
