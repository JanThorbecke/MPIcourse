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

  IF ( myid < ntasks-1 ) THEN
    CALL MPI_SEND(message,size, MPI_INTEGER, myid+1, &
        myid+1, MPI_COMM_WORLD, ierror)
    WRITE(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, ' Sent elements: ',size, &
        '. Tag: ', myid+1, '. Receiver: ', myid+1

  END IF

  IF ( myid > 0 ) THEN
    
    CALL MPI_RECV(message,size, MPI_INTEGER,MPI_ANY_SOURCE,  &
        MPI_ANY_TAG, MPI_COMM_WORLD, status, ierror)
    CALL MPI_GET_COUNT(status,MPI_INTEGER,count,ierror)
    WRITE(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Receiver: ', myid, 'received elements: ',count, &
        ' Tag: ', status(MPI_TAG), &
        ' Sender: ', status(MPI_SOURCE)
  END IF

  CALL MPI_FINALIZE(ierror)

END PROGRAM basic
