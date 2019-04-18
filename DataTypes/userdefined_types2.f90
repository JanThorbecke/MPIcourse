PROGRAM usertypes2

  IMPLICIT NONE
  INCLUDE 'mpif.h'

  INTEGER, PARAMETER :: n = 4
  REAL(kind=4), DIMENSION(n,n) :: array
  INTEGER, DIMENSION(n) :: displ, blockl

  INTEGER :: i,j
  INTEGER :: my_id, ntasks, rc
  INTEGER :: newtype

  CALL MPI_INIT(rc)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, rc)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_id, rc)

  IF (my_id ==0 ) THEN
     DO i=1,n
        DO j=1,n
           array(i,j) = i*10 + j
        END DO
     END DO
  ELSE
     array(:,:) = 0.0
  END IF

  DO i=1,n
     displ(i) = n*(i-1) + (i-1)
     blockl(i) = n - (i - 1)
  END DO

  CALL MPI_TYPE_INDEXED(n, blockl, displ, MPI_REAL, newtype, rc)
  CALL MPI_TYPE_COMMIT(newtype, rc)

! Send the lower triangle from 0 to 1
  IF (my_id ==0 ) THEN
     CALL MPI_SEND(array, 1, newtype, 1, 0, MPI_COMM_WORLD, rc)
  ELSE
     CALL MPI_RECV(array, 1, newtype, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, rc)
  END IF

  IF (my_id ==1 ) THEN
     DO i=1,n
        WRITE(*,*) array(i,:)
     END DO
  END IF

  CALL MPI_TYPE_FREE(newtype, rc)
  CALL MPI_FINALIZE(rc)

END PROGRAM
