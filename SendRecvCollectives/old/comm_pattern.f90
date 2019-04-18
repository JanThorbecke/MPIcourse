PROGRAM comm_pattern
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  INTEGER,PARAMETER::chunk_size=100
  INTEGER,PARAMETER::root=1

  INTEGER::ierror, myid,ntasks,count
  INTEGER,ALLOCATABLE:: buf(:),recvbuf(:)
  

  CALL MPI_INIT(ierror)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierror)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, ierror)
  
  ALLOCATE(buf(chunk_size*ntasks))
  ALLOCATE(recvbuf(chunk_size*ntasks))
  
  CALL init_array(buf)
  CALL broadcast(buf,chunk_size,root)
  CALL check_broadcast(buf,root)


  CALL init_array(buf)
  CALL gather(buf,chunk_size,recvbuf,root)
  CALL check_gather(recvbuf,root)

  CALL init_array(buf)
  CALL scatter(buf,chunk_size,recvbuf,root)
  CALL check_scatter(recvbuf,root)
  

  DEALLOCATE(buf)
  DEALLOCATE(recvbuf)
  CALL MPI_FINALIZE(ierror)
CONTAINS


SUBROUTINE broadcast(buffer,count,root)
   ! write this routine 
   INTEGER, DIMENSION(count),INTENT(INOUT):: buffer
   INTEGER, INTENT(IN):: root,count

END SUBROUTINE broadcast

SUBROUTINE scatter(sendbuffer,count,recvbuffer,root)
  ! write this routine 
  INTEGER, DIMENSION(:),INTENT(IN):: sendbuffer
  INTEGER, DIMENSION(:),INTENT(OUT):: recvbuffer
  INTEGER, INTENT(IN):: root,count

END SUBROUTINE scatter

SUBROUTINE gather(sendbuffer,count,recvbuffer,root)
  ! write this routine 
  INTEGER, DIMENSION(:),INTENT(IN):: sendbuffer
  INTEGER, DIMENSION(:),INTENT(OUT):: recvbuffer
  INTEGER, INTENT(IN):: root,count

 END SUBROUTINE gather
 


!below this point some helper routines are defined

SUBROUTINE init_array(buffer)
  INTEGER, DIMENSION(:),INTENT(INOUT):: buffer
  INTEGER::i

  DO i=1,SIZE(buffer)
    buffer(i)=myid+(i-1)*ntasks
  END DO
  
END SUBROUTINE init_array


SUBROUTINE print_errors(label,errorin,root)
  INTEGER,INTENT(IN):: errorin,root
  CHARACTER(LEN=*):: label
  INTEGER::i,error
  IF ( myid .EQ. 0) THEN 
    error=errorin
    DO i = 0,ntasks-1
      IF (i>0) THEN
        CALL MPI_RECV(error,1,MPI_INTEGER,i,MPI_ANY_TAG,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierror)
      END IF
      IF (error .NE. 0) THEN
        WRITE(*,*) label,":",error," errors for rank ",i
      ELSE
        WRITE(*,*) label,": success for rank ",i
      END IF
    END DO
  ELSE
    CALL MPI_SEND(errorin,1,MPI_INTEGER,0,10,MPI_COMM_WORLD,ierror)
  END IF

END SUBROUTINE print_errors

SUBROUTINE  check_broadcast(buffer,root)
  INTEGER, DIMENSION(:),INTENT(INOUT):: buffer
  INTEGER, INTENT(IN):: root
  INTEGER::i,error  
  error=0
  DO i=1,chunk_size
    IF (buffer(i) .NE. root+(i-1)*ntasks) THEN
      error=error+1
    END IF
  END DO
  CALL print_errors("broadcast",error,root);
END SUBROUTINE check_broadcast

SUBROUTINE check_scatter(buffer,root)
  INTEGER, DIMENSION(:),INTENT(INOUT):: buffer
  INTEGER, INTENT(IN):: root
  INTEGER::i,error
  error=0
  DO i=1,chunk_size
    IF (buffer(i) .NE. root+(myid*chunk_size+(i-1))*ntasks) THEN
      error=error+1
    END IF
  END DO
  CALL print_errors("scatter",error,root);
END SUBROUTINE check_scatter

SUBROUTINE check_gather(buffer,root)
 INTEGER, DIMENSION(:),INTENT(INOUT):: buffer
 INTEGER, INTENT(IN):: root
 INTEGER::p,i,error
 
 error=0
 IF (myid .EQ. root) THEN
   DO p=0,ntasks-1
     DO i=1,chunk_size
       IF (buffer(i+p*chunk_size) .NE. p+(i-1)*ntasks) THEN
         error=error+1
       END IF
     END DO
   END DO
 END IF
 CALL print_errors("gather",error,root);
END SUBROUTINE check_gather

END PROGRAM comm_pattern

