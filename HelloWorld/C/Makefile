EXECS=hello_world
MPICC?=mpicc

all: ${EXECS}

hello_world: hello_world.c
	${MPICC} -o hello_world hello_world.c

clean:
	rm ${EXECS}

