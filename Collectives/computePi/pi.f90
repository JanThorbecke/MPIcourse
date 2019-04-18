PROGRAM integrate_pi
  USE mpi
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12), root_id = 0
  REAL(KIND=dp) :: mypi, pi, h, sum, x, t, t0
  INTEGER :: n, i

! Init MPI environment
  CALL MPI_INIT(rc)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_id, rc)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, rc)

! Ask for the integration interval
  IF (my_id == root_id) THEN
     WRITE(*,*) 'Enter the number of intervals (0 quits):'
     READ(*,*) n
     IF (n == 0) STOP 'user request'
  END IF

! measure MPI wall time
  t0 = MPI_WTIME()

! BROADCAST n TO OTHER PROCESSES HERE

! The function to integrate: f(x)=4/(1+x^2)
! Divide the integration into #tasks slices
  h = 1.0_dp / REAL(n,dp)
  sum = 0.0_dp
  DO i = my_id+1, n, ntasks
    x = h * (REAL(i,dp) - 0.5_dp)
    sum = sum + (4.0_dp / (1.0_dp + x*x))
  END DO
  mypi = h * sum

! COLLECT ALL THE PARTIAL SUMS TO THE ROOT PROCESS HERE

  t = MPI_WTIME() - t0

! Root node prints the answer
  IF (my_id == root_id) THEN
     WRITE(*,*) 'Obtained approximation for pi is ', pi
     WRITE(*,*) 'Pi to 25 decimal places is 3.141592653589793238462643'
     WRITE(*,*) 'Evaluation took', t, 'seconds with ', ntasks, ' &
          tasks.'
  END IF

! Finalize MPI
  CALL MPI_FINALIZE(rc)
END PROGRAM integrate_pi
