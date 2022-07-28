EXECS=hello
MPIF90?=mpiifort
MPIF90=mpif90

all: ${EXECS}

hello: hello.f90
	${MPIF90} -o hello hello.f90

clean:
	rm ${EXECS}

