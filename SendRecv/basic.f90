PROGRAM basic

  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER,PARAMETER::size=100
  INTEGER::ierror, myid,ntasks,count
  INTEGER:: status(MPI_STATUS_SIZE)
  INTEGER:: message(size)
  
  CALL MPI_INIT(ierror)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierror)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, ierror)

  message = myid

!Send and receive as defined in exercise

  CALL MPI_FINALIZE(ierror)

END PROGRAM basic
