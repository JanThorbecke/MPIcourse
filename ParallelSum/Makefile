MPICC=mpicc
CC=cc

EXECS=sum

all: ${EXECS}

sum: sum.c
	${MPICC} -o $@ $<

sum_serial: sum_serial.c
	${CC} -o $@ $<

sum_getcount: sum_getcount.c
	${MPICC} -o $@ $<

sum_probe: sum_probe.c
	${MPICC} -o $@ $<

clean:
	rm sum sum_serial sum_getcount sum_probe *.o

