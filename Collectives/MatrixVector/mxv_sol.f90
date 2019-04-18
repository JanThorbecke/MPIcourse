program simple_matrix_vector_multiply
  ! computes Ax=y in parallel
  use mpi
  implicit none
  integer, parameter :: n = 512, root = 0, tag = 10
  real, dimension(:,:), allocatable :: A, Aloc
  real, dimension(:), allocatable :: x, y, yloc
  integer, dimension(MPI_STATUS_SIZE) :: status
  integer :: ndim, i, low, up, my_id, rc, ntasks

  call mpi_init(rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)
  call mpi_comm_rank(MPI_COMM_WORLD, my_id, rc)

  ndim = n*ntasks
  allocate(Aloc(n,ndim), yloc(n), x(ndim), y(ndim))

  if (my_id == root) then
     allocate (A(ndim,ndim))
     call random_number(A)
     call random_number(x)
! scatter written explicitly with sends/recvs
     low = n+1
     up = 2*n
     do i = 1, ntasks-1
        call mpi_send(A(low:up,:), n*ndim, MPI_REAL, i, tag, &
             MPI_COMM_WORLD, rc)
        low = up + 1
        up = up + n
     end do
     do i = 1, n
        Aloc(i,:) = A(i,:)
     end do
  else
     call mpi_recv(Aloc, n*ndim, MPI_REAL, root, &
          MPI_ANY_TAG, MPI_COMM_WORLD, status, rc)
  end if

! Alternative written MPI_Scatter()
!  call MPI_Scatter(A, n*ndim, MPI_FLOAT, Aloc, n*ndim, MPI_FLOAT, root, MPI_COMM_WORLD, rc);

! transmit x to all processes
  call mpi_bcast(x, ndim, MPI_REAL, root, MPI_COMM_WORLD, rc)

  yloc = matmul(Aloc,x)

! combine the full y and make it available to all
! tasks

  call mpi_allgather(yloc, n, MPI_REAL, y, n, MPI_REAL, &
       MPI_COMM_WORLD, rc)
  
  if (my_id == root) write(*,*) y(ndim)

  call mpi_finalize(rc)

end program simple_matrix_vector_multiply
