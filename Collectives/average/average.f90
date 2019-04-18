PROGRAM average2
  USE mpi 
  IMPLICIT NONE
  INTEGER, PARAMETER :: root_id = 0 
  REAL, DIMENSION(:), ALLOCATABLE :: &
       a, & ! Table in the root node
       aloc ! Local table
  INTEGER, DIMENSION(:), ALLOCATABLE :: &
       send_counts, & ! Number of numbers
       displs ! starting locations
  INTEGER :: &
       data_size, & ! Size of data
       my_num, & ! Number of data points in a task
       allocstat, i, local_nr, ival_mod 
  REAL :: psum, total_sum 

  CALL MPI_INIT(rc)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, rc)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_id, rc)
  IF (my_id == root_id) THEN
     WRITE (*,*) 'Give the number of datapoints, zero or negative quits'
     READ (*,*) data_size
     IF (data_size <= 0) STOP 'User request'
  END IF

! BROADCAST THE NUMBER OF DATAPOINTS TO ALL TASKS HERE
!
!

! Calculate the local amounts, some tasks are going to get
! one point more than the rest
  local_nr = data_size/ntasks
  ival_mod = MOD(data_size,ntasks)
  IF (my_id < ival_mod) THEN
     my_num = local_nr+1
  ELSE
     my_num = local_nr
  END IF
  ALLOCATE(aloc(my_num), STAT=allocstat)
  IF (allocstat /= 0) STOP 'memory allocation failed'
  WRITE (*,*) 'I am ', my_id, 'and I have ', my_num, ' data points.'

! Treat the root node separately: calculate the data portions
! to deliver and generate the data set
  IF (my_id == root_id) THEN
     ALLOCATE(send_counts(0:ntasks-1), displs(0:ntasks-1), &
          STAT=allocstat)
     ALLOCATE(a(data_size), STAT=allocstat)
     IF (allocstat /= 0) STOP 'Memory allocation failed'
     CALL RANDOM_NUMBER(a)

! DETERMINE THE SEND COUNTS AND DISPLACEMENTS HERE
!
!
!

  END IF

! DELIVER THE NUMBERS TO TASKS HERE
!
!
!

! Calculate local sum
  psum=SUM(aloc)

! GATHER THE LOCAL SUM INTO A GLOBAL SUM HERE
!
!
!

! The root node prints the results
  IF (my_id == root_id) THEN
     WRITE (*,*) 'Average is ', total_sum/REAL(data_size)
  END IF

  CALL MPI_FINALIZE(rc)
END PROGRAM average2
