EXECS=hello_world version_test
MPICC?=mpicc

all: ${EXECS}

hello_world: hello_world.c
	${MPICC} -o hello_world hello_world.c

version_test: version_test.c
	${MPICC} -o $@ $<

clean:
	rm ${EXECS}

