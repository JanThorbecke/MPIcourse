PROGRAM deadlock
  USE mpi
  IMPLICIT NONE
  INTEGER,PARAMETER::size=10000000
  INTEGER :: err, myid,ntasks,i, n, m, count, dtype
  INTEGER :: stat(mpi_status_size,2),req(2)
  DOUBLE PRECISION :: outbuf(size),inbuf(size)

  CALL mpi_init(err)
  CALL mpi_comm_rank(mpi_comm_world,myid,err)
  CALL mpi_comm_size(mpi_comm_world,ntasks,err)


  DO m=1,7
    
    n=10**m

    IF ( myid == 0 ) THEN
      outbuf=0
      DO i=1,ntasks-1
         CALL mpi_isend(outbuf,n,mpi_double_precision,i,0,mpi_comm_world,req(1),err)
         CALL mpi_irecv(inbuf,n,mpi_double_precision,i,1,mpi_comm_world,req(2),err)
         CALL mpi_waitall(2,req,stat,err)
         CALL mpi_get_count(stat(:,1),mpi_double_precision,count,err)
         PRINT *,'0 received',count,'numbers'
      end DO
    ELSE
      outbuf=1
      CALL mpi_irecv(inbuf,n,mpi_double_precision,0,0,mpi_comm_world,req(1),err)
      CALL mpi_isend(outbuf,n,mpi_double_precision,0,1,mpi_comm_world,req(2),err)
      CALL mpi_waitall(2,req,stat,err)
      CALL mpi_get_count(stat(:,1),mpi_double_precision,count,err)
      PRINT *,myid,' received',count,'numbers'
    END IF

  END DO

  CALL mpi_finalize(err)

END PROGRAM deadlock
