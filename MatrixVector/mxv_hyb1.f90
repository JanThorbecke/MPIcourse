program mxv_mpi
  ! computes Ax=y in parallel, simple MPI+OpenMP parallelization
  ! "fine grained" thread parallelization, over rows with MPI
  ! and over columns with OpenMP
  ! PM/CSC for the course Parallel Programming (Feb 15-18, 2011) at CSC
  use omp_lib
  implicit none
  include 'mpif.h'
  real(8), dimension(:,:), allocatable :: A, Aloc
  real(8), dimension(:), allocatable :: x, y, yloc
  integer, dimension(:), allocatable :: nlocrows, recvcnts, displs
  integer, dimension(MPI_STATUS_SIZE) :: status
  real(8) :: t0, t
  integer, parameter :: root = 0, tag = 10
  integer :: n, nloc, i, j, low, up, nrows, nmod, ncols, ncmod
  integer :: rc, my_id, ntasks, my_thr, nthreads

  call mpi_init(rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)
  call mpi_comm_rank(MPI_COMM_WORLD, my_id, rc)

  if (my_id==root) then
    read(*,*) n
    allocate(recvcnts(0:ntasks-1), displs(0:ntasks-1), &
        nlocrows(0:ntasks-1))
    nrows=n/ntasks
    nmod=mod(n,ntasks)
    do i = 0, nmod-1
      nlocrows(i) = nrows+1
    end do
    do i = nmod, ntasks-1
      nlocrows(i) = nrows
    end do
  end if
  call mpi_bcast(n,1,MPI_INTEGER,root,MPI_COMM_WORLD,rc)
  call mpi_scatter(nlocrows,1,MPI_INTEGER,nloc,1,MPI_INTEGER,root,&
      MPI_COMM_WORLD,rc)
  
  allocate(yloc(nloc), x(n))
  allocate(Aloc(nloc,n))

  if (my_id==root) then
    allocate(A(n,n),y(n))
    y = 0.0
    forall (i=1:n) x(i) = 0.25*i
    forall (i=1:n, j=1:n) A(i,j) = 1.0/real(i+j-1,8)
  end if

  t0 = mpi_wtime()
  call mpi_bcast(x,n,MPI_DOUBLE_PRECISION,root,MPI_COMM_WORLD,rc)

! scatterv written explicitly with sends/recvs
  if (my_id == root) then
    ! those of the root process
    do i = 1, nloc
      Aloc(i,:) = A(i,:)
    end do
    ! the rest of the processes
    up = nloc
    do i = 1, ntasks-1
      low = up + 1
      up = low + nlocrows(i) - 1      
      call mpi_send(A(low:up,:), n*nlocrows(i), MPI_DOUBLE_PRECISION, i, &
          tag, MPI_COMM_WORLD, rc)
    end do
  else
     call mpi_recv(Aloc, nloc*n, MPI_DOUBLE_PRECISION, root, &
          tag, MPI_COMM_WORLD, status, rc)
  end if

  ! local mxv
!$OMP PARALLEL DO SHARED(Aloc,x,n,nloc) PRIVATE(i,j) REDUCTION(+:yloc)
  do j = 1, n
    do i = 1, nloc
      yloc(i) = yloc(i) + Aloc(i,j)*x(j)
    end do
  end do
!$OMP END PARALLEL DO

! combine the full y to the root process
  if (my_id==root) then
    displs(0) = 0
    do i = 1, ntasks-1
      displs(i) = displs(i-1) + nlocrows(i-1)
    end do
  end if
  call mpi_gatherv(yloc, nloc, MPI_DOUBLE_PRECISION, y, nlocrows, &
      displs, MPI_DOUBLE_PRECISION, root, MPI_COMM_WORLD, rc)
 
  t = mpi_wtime() - t0
  if (my_id == root) then
    open(10,file='mxv_hyb1.dat',status='unknown',position='append')
    write(10,'(I8,F10.3,G10.3,G10.4,F12.4)') n, y(n), t, &
        (real(2*n*n,8)/t)*1e-6
    close(10)
  end if

  call mpi_finalize(rc)

end program mxv_mpi
