PROGRAM pingpong

  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER, PARAMETER :: proc_A=0, proc_B=1
  INTEGER :: ierror, id, SIZE
      
  CALL MPI_INIT(ierror)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, id, ierror)
  IF (id == proc_A) THEN
    CALL processor_A()
  ELSE IF (id == proc_B) THEN
    CALL processor_B()
  END IF
  CALL MPI_FINALIZE(ierror)
  
CONTAINS
      
  SUBROUTINE processor_A()

    INTEGER, PARAMETER :: ping=101, pong=102
    INTEGER :: i, ierror, length, status(MPI_STATUS_SIZE)
    REAL(8) :: start, finish, time
    DOUBLE PRECISION, DIMENSION(100001) :: array

    array = 0.0
    DO length = 1, 100001, 1000
      start = MPI_WTIME()
      DO i = 1,10
 !ping pong communication

      END DO
      finish = MPI_WTIME()
      time = finish - start
      WRITE(*,'(A,I10,A,A,E12.4,A,A,E12.4,A)') 'Size  ', length*8, ' (bytes) ', &
          ' Time   ', time/10, ' (s) ',&
          ' Bandwidth  ', REAL(2 * 8 * 10 * length)/time, &
          ' bytes/s'
    END DO

  END SUBROUTINE processor_A
      
  SUBROUTINE processor_B()

    INTEGER, PARAMETER :: ping=101, pong=102
    INTEGER :: i, ierror, length, status(MPI_STATUS_SIZE)
    DOUBLE PRECISION, DIMENSION(100001) :: array

    DO length = 1, 100001, 1000
      DO i = 1,10
 !ping pong communication        
      END DO
    END DO

  END SUBROUTINE processor_B 

END PROGRAM pingpong

