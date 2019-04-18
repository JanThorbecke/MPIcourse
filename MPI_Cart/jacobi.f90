program jacobi
! solves \nabla^2 u = 1.0 using the Jacobi iteration
  implicit none
  intrinsic sqrt, nint
  include 'mpif.h'
  real(8), allocatable, dimension(:,:) :: a, b, f
  real(8) :: diffnorm=1.0, dwork, tolerance=5.E-7, h, t2, t1
  integer, parameter :: max_it=10000, & ! maximum iterations
                        nx = 250, ny = 250 ! grid points in x and y dirs
  integer :: rc, it, my_id, ntask, npx, npy, npdiff, sc, ec, sr, er, &
      comm2d, nbr_left, nbr_right, nbr_down, nbr_up, bndrytype
  integer :: status(mpi_status_size),dims(2),coords(2)
  logical, dimension(2):: period
  
  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world,ntask,rc)
  call mpi_comm_rank(mpi_comm_world,my_id,rc)

! Processor grid, aiming at sqrt(N) x sqrt(N) grid

  npx=int(sqrt(real(ntask)))
  npy=ntask/npx
  npdiff = ntask-(npx*npy)
  npy = npy + npdiff

! Define 2d cartesian npx x npy processor topology

  dims(1)=npx
  dims(2)=npy
  period= (/ .false.,.false. /)
  
  call mpi_cart_create(MPI_COMM_WORLD,2,dims,period,.true.,comm2d, rc)
  call mpi_cart_shift(comm2d,1,1,nbr_left,nbr_right,rc)
  call mpi_cart_shift(comm2d,0,1,nbr_down,nbr_up,rc) 

  call mpi_comm_rank(comm2d,my_id,rc)
  call mpi_cart_coords(comm2d,my_id,2,coords,rc)

! Find index limits for processors

  call decomp(nx,npx,coords(1),sr,er)
  call decomp(ny,npy,coords(2),sc,ec)

  if (my_id==0) write(*,'(A,A)') 'Coordinate in the processor grid of each ',&
       'processor and the indeces of the domain'
  call mpi_barrier(MPI_COMM_WORLD,rc)
  write(*,'(I2,A2,I2,A1,I2,A2,4I4)') &
       my_id,' (',coords(1),',',coords(2),')',sr,er,sc,ec

! Define an MPI datatype for a 2d array row

  call mpi_type_vector(ec-sc+1,1,er-sr+3,mpi_double_precision,&
      & bndrytype,rc)
  call mpi_type_commit(bndrytype,rc)
  allocate(a(sr-1:er+1,sc-1:ec+1),&
      b(sr-1:er+1,sc-1:ec+1), &
      f(sr-1:er+1,sc-1:ec+1))

! Initialize arrays

  call init(f,nx,sr,er,sc,ec)
  f=1.0d0
  h=1.d0/real(nx+1,8)
  it=0
  ! iteration loop
  if (my_id==0) write (*,*) ' '
  call mpi_barrier(MPI_COMM_WORLD,rc)
  t1 = mpi_wtime()
  do while ( (diffnorm > tolerance) .and. it < max_it )
    it=it+1
    ! Halo exchange
    call exchng(a,sc,ec,sr,er,comm2d,nbr_left,nbr_right,nbr_up,nbr_down)
    ! Update a->b
    call sweep(a,f,sr,er,sc,ec,nx,b,h)
    ! Halo exchange
    call exchng(b,sc,ec,sr,er,comm2d,nbr_left,nbr_right,nbr_up,nbr_down)
    ! Update b->a
    call sweep(b,f,sr,er,sc,ec,nx,a,h)
    ! Local difference between latest iterates
    dwork = diff(a,b,sr,er,sc,ec)
    ! Sum local differences
    call mpi_allreduce(dwork,diffnorm,1,mpi_double_precision,mpi_sum,&
        & comm2d,rc)
    if (my_id==0) then
      if (mod(it,200)==0) write(*,'(A,I5,A,G10.3)') &
           'iteration=', it, ' norm=', diffnorm
    endif
  end do
  t2 = mpi_wtime() - t1

  if (my_id==0) then
    if (it < max_it) then
      write(*,*) 'Done in ',it, 'iterations, norm ', diffnorm
    else
      write(*,*) 'Not converged, norm=', diffnorm
    end if
    write(*,*) 'Wall time ', t2
  end if
  
  call mpi_finalize(rc)

contains

  subroutine decomp(n,ntask,my_id,s,e)
    implicit none
    integer, intent(in) :: n,ntask,my_id
    integer, intent(out) :: s, e
    integer :: deficit,nlocal
    intrinsic MIN, MOD

    nlocal = n/ntask
    s = my_id*nlocal + 1
    deficit = mod(n,ntask)
    s = s + min(my_id,deficit)
    if (my_id<deficit) then
      nlocal = nlocal + 1
    end if
    e = s + nlocal - 1
    if (e>n .or. my_id==ntask-1) e = n
    return
  end subroutine decomp

  function diff(a,b,sr,er,sc,ec)
! squares difference
    implicit none
    real(8):: diff
    integer :: er,ec,nx,sr,sc
    real(8), dimension(sr-1:er+1,sc-1:ec+1) :: a,b
    real(8) :: s
    integer :: i,j
                                                                           
    s = 0.0d0
    do j = sc, ec
      do i = sr, er
         s = s + (a(i,j)-b(i,j))**2
      end do
    end do
    diff = s
    return
  end function diff

  subroutine exchng(a,sc,ec,sr,er,comm2d,nbr_left,nbr_right,nbr_up,nbr_down)
! Halo exchange
    implicit none
    integer :: comm2d,ec,er,nbr_up,nbr_down,nx,sc,sr,nbr_left,nbr_right, &
        col_len
    real(8), dimension (sr-1:er+1,sc-1:ec+1) :: a
    
    col_len=er-sr+1
    call mpi_sendrecv(a(sr,sc),col_len,mpi_double_precision,nbr_left,0,&
         a(sr,ec+1),col_len,mpi_double_precision,nbr_right,&
         0,comm2d,status,rc)
    call mpi_sendrecv(a(sr,ec),col_len,mpi_double_precision,nbr_right,1,&
         a(sr,sc-1),col_len,mpi_double_precision,nbr_left,1,comm2d,status,rc)
    call mpi_sendrecv(a(sr,sc),1,bndrytype,nbr_down,2,a(er+1,sc),1,&
         bndrytype,nbr_up,2,comm2d,status,rc)
    call mpi_sendrecv(a(er,sc),1,bndrytype,nbr_up,3,a(sr-1,sc),1,&
         bndrytype,nbr_down,3,comm2d,status,rc)
  end subroutine exchng

  subroutine init(f,nx,sr,er,sc,ec)
! define the values of function f 
    implicit none
    integer :: e,nx,sr,er,sc,ec
    real (8), dimension (sr-1:er+1,sc-1:ec+1) :: f
    real (8):: x,y
    integer :: i, j
    
    do i=sc,ec
      do j=sr,er
        x=real(i,8)/real(nx+1,8)
        y=real(j,8)/real(nx+1,8)
!        f(j,i) = sin(x*y)
        f(j,i)=exp(-(x-0.5)**2-(y-0.5)**2)
      end do
    end do
  end subroutine init

  subroutine sweep(V_old,f,sr,er,sc,ec,nx,V_new,h)
! Jacobi iteration
    implicit none                                          
    integer, intent(in) :: sr,er,sc,ec,nx
    real(8), dimension (sr-1:er+1,sc-1:ec+1), intent(in) :: f
    real(8), dimension (sr-1:er+1,sc-1:ec+1), intent(inout) :: V_old, V_new
    real(8) :: h
    integer :: i,j
    
    do j = sc,ec
       do i = sr, er
          V_new(i,j) = 0.25*(V_old(i-1,j)+V_old(i,j+1)+ &
              V_old(i,j-1)+V_old(i+1,j)-h*h*f(i,j))
       end do
    end do
  end subroutine sweep

end program jacobi



