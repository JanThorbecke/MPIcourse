      program main
      use mpi
      double precision  PI25DT
      parameter (PI25DT = 3.141592653589793238462643d0)
      double precision  mypi, pi, h, sum, x, a
      integer n, myid, numprocs, i, ierr

!     function to integrate       f(a) = 4.d0 / (1.d0 + a*a)

      call MPI_INIT( ierr )
      call MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
      call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )

 10   if ( myid .eq. 0 ) then
         n=1000000000;
      endif

      call MPI_BCAST( n, 1, MPI_INTEGER, 0, &
     &                MPI_COMM_WORLD, ierr)

!    calculate the interval size
      h = 1.0d0/n
      sum  = 0.0d0
      do i = myid+1, n, numprocs
        x   = h * (dble(i) - 0.5d0)
        sum = sum + 4.0 / (1.0 + x*x)
      enddo

      mypi = h * sum

!     collect all the partial sums
      call MPI_REDUCE( mypi, pi, 1, MPI_DOUBLE_PRECISION, &
     &                MPI_SUM, 0, MPI_COMM_WORLD,ierr)

!     node 0 prints the answer
      if (myid .eq. 0) then
         write(6, 97) pi, abs(pi - PI25DT)
 97      format('  pi is approximately: ', F18.16, &
     &          '  Error is: ', F18.16)
      endif

 30   call MPI_FINALIZE(ierr)
      end


