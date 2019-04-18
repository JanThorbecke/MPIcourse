PROGRAM mxv_mpi
  ! computes Ax=y in parallel, simple MPI+OpenMP parallelization
  ! parallelization over rows with MPI, over columns with threads
  ! PM/CSC for the course Parallel Programming (Feb 15-18, 2011) at CSC
  USE omp_lib
  IMPLICIT NONE
  INCLUDE 'mpif.h'
  REAL(8), DIMENSION(:,:), ALLOCATABLE :: A, Aloc
  REAL(8), DIMENSION(:), ALLOCATABLE :: x, y, yloc
  INTEGER, DIMENSION(:), ALLOCATABLE :: nlocrows, recvcnts, displs
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
  REAL(8) :: t0, t
  INTEGER, PARAMETER :: root = 0, tag = 10
  INTEGER :: n, nloc, i, j, low, up, nrows, nmod, ncols, ncmod
  INTEGER :: rc, my_id, ntasks, my_thr, nthreads

  CALL mpi_init(rc)
  CALL mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)
  CALL mpi_comm_rank(MPI_COMM_WORLD, my_id, rc)

  IF (my_id==root) THEN
    READ(*,*) n
    ALLOCATE(recvcnts(0:ntasks-1), displs(0:ntasks-1), &
        nlocrows(0:ntasks-1))
    nrows=n/ntasks
    nmod=MOD(n,ntasks)
    DO i = 0, nmod-1
      nlocrows(i) = nrows+1
    END DO
    DO i = nmod, ntasks-1
      nlocrows(i) = nrows
    END DO
  END IF
  CALL mpi_bcast(n,1,MPI_INTEGER,root,MPI_COMM_WORLD,rc)
  CALL mpi_scatter(nlocrows,1,MPI_INTEGER,nloc,1,MPI_INTEGER,root,&
      MPI_COMM_WORLD,rc)
  
  ALLOCATE(yloc(nloc), x(n))
  ALLOCATE(Aloc(nloc,n))

  IF (my_id==root) THEN
    ALLOCATE(A(n,n),y(n))
    y = 0.0
    FORALL (i=1:n) x(i) = 0.25*i
    FORALL (i=1:n, j=1:n) A(i,j) = 1.0/REAL(i+j-1,8)
  END IF

  t0 = mpi_wtime()
  CALL mpi_bcast(x,n,MPI_DOUBLE_PRECISION,root,MPI_COMM_WORLD,rc)

! scatterv written explicitly with sends/recvs
  IF (my_id == root) THEN
    ! those of the root process
    DO i = 1, nloc
      Aloc(i,:) = A(i,:)
    END DO
    ! the rest of the processes
    up = nloc
    DO i = 1, ntasks-1
      low = up + 1
      up = low + nlocrows(i) - 1      
      CALL mpi_send(A(low:up,:), n*nlocrows(i), MPI_DOUBLE_PRECISION, i, &
          tag, MPI_COMM_WORLD, rc)
    END DO
  ELSE
     CALL mpi_recv(Aloc, nloc*n, MPI_DOUBLE_PRECISION, root, &
          tag, MPI_COMM_WORLD, status, rc)
  END IF

  ! local mxv
!$OMP PARALLEL SHARED(Aloc,x,n,nloc,nthreads, ncols, ncmod) &
!$OMP & PRIVATE(i,j, my_thr, up, low) REDUCTION(+:yloc)
  my_thr = omp_get_thread_num()
  nthreads = omp_get_num_threads()
  ncols = n/nthreads
  ncmod = MOD(n,nthreads)
  IF (my_thr < nmod) THEN
    low = my_thr*(ncols+1) + 1
    up = low + ncols
  ELSE
    low = my_thr*ncols + 1
    up = low + ncols - 1
  END IF

  DO j = low, up
    DO i = 1, nloc
      yloc(i) = yloc(i) + Aloc(i,j)*x(j)
    END DO
  END DO
!$OMP END PARALLEL

! combine the full y to the root process
  IF (my_id==root) THEN
    displs(0) = 0
    DO i = 1, ntasks-1
      displs(i) = displs(i-1) + nlocrows(i-1)
    END DO
  END IF
  CALL mpi_gatherv(yloc, nloc, MPI_DOUBLE_PRECISION, y, nlocrows, &
      displs, MPI_DOUBLE_PRECISION, root, MPI_COMM_WORLD, rc)
 
  t = mpi_wtime() - t0
  IF (my_id == root) THEN
    OPEN(10,file='mxv_hyb2.dat',status='unknown',position='append')
    WRITE(10,'(I8,F10.3,G10.3,G10.4,F12.4)') n, y(n), t, &
        (REAL(2*n*n,8)/t)*1e-6
    CLOSE(10)
  END IF

  CALL mpi_finalize(rc)

END PROGRAM mxv_mpi
