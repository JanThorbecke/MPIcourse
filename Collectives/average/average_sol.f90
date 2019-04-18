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
  CALL MPI_BCAST(data_size, 1, MPI_INTEGER, 0, &
       MPI_COMM_WORLD, rc)

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
     IF (allocstat /= 0) STOP 'Memory allocation failed'
     send_counts(0:ival_mod-1) = local_nr+1
     send_counts(ival_mod:) = local_nr
     displs(0) = 0
     DO i = 0, ntasks-2
        displs(i+1) = displs(i) + send_counts(i)
     END DO
     WRITE (*,*) 'Number of datapoints ', send_counts
     ALLOCATE(a(data_size), STAT=allocstat)
     IF (allocstat /= 0) STOP 'Memory allocation failed'
     CALL RANDOM_NUMBER(a)
  END IF

! DELIVER THE NUMBERS TO TASKS HERE
  CALL MPI_SCATTERV(a, send_counts, displs, MPI_REAL, &
       aloc, my_num, MPI_REAL, root_id, MPI_COMM_WORLD, rc)

! Calculate local sum
  psum=SUM(aloc)

! GATHER THE LOCAL SUM INTO A GLOBAL SUM HERE
  CALL MPI_REDUCE(psum, total_sum, 1, MPI_REAL, MPI_SUM, &
       root_id, MPI_COMM_WORLD, rc)

! The root node prints the results
  IF (my_id == root_id) THEN
     WRITE (*,*) 'Average is ', total_sum/REAL(data_size)
  END IF

  CALL MPI_FINALIZE(rc)
END PROGRAM average2
