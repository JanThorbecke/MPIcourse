
/************************************************************************
 * This file has been written as a sample solution to an exercise in a 
 * course given at the Edinburgh Parallel Computing Centre. It is made
 * freely available with the understanding that every copy of this file
 * must include this header and that EPCC takes no responsibility for
 * the use of the enclosed teaching material.
 *
 * Author:      Joel Malard, Rolf Rabenseifner     
 *
 * Contact:     epcc-tec@epcc.ed.ac.uk, rabenseifner@hlrs.de
 *
 * Purpose:     A program to try out non-blocking point-to-point 
 *              communications.
 *
 * Contents:    C source code.
 *
 ************************************************************************/

#include	<stdio.h>
#include	<mpi.h>
#define tag_to_right 201
#define MAXDIMS 2 

void main (int argc, char *argv[])
{
int ierror, snd_buf, my_rank, size;
int right, left;
int top, bottom;
int rcv_buf, sum, i;

MPI_Comm    new_comm;
int         dims[MAXDIMS],
            periods[MAXDIMS],
            reorder,
            coords[MAXDIMS];

MPI_Status  recv_status;

    MPI_Init(&argc, &argv);

    /* Get process info. */

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    /* Set cartesian topology. */

    /* integer array of size ndims specifying the number of processes in each dimension */
    /* A value of 0 indicates that MPI_Dims_create should fill in a suitable value. */
    dims[0] = 0; 
    dims[1] = 0; 
    MPI_Dims_create(size, MAXDIMS, dims); 

    periods[0] = 1; /* periodic in x-direction (left-right)*/
    periods[1] = 0; /* non-periodic in y-direction (top-bottom)*/
    reorder = 1; /* ranking may be reordered */
    MPI_Cart_create(MPI_COMM_WORLD,MAXDIMS,dims,periods,reorder,&new_comm);

    MPI_Comm_rank(new_comm, &my_rank);
    /* Determines process coords in cartesian topology given rank in group */
    MPI_Cart_coords(new_comm, my_rank, MAXDIMS, coords); 

    /* Get nearest neighbour ranks. */
    MPI_Cart_shift(new_comm, 0, 1, &left, &right);
    MPI_Cart_shift(new_comm, 1, 1, &top, &bottom);
 
    /* Compute global sum. */
    sum = 0;
    snd_buf = my_rank;

    for( i = 0; i < dims[0]; i++) {

        MPI_Sendrecv(&snd_buf,  1, MPI_INT, right, tag_to_right,
                     &rcv_buf, 1, MPI_INT, left,  tag_to_right,
                     new_comm, &recv_status);
    
        sum = sum + rcv_buf;
        snd_buf = rcv_buf;
    
    }

    printf ("PE-%3d: x=%d, y=%d: Sum = %d neighbors: left=%d right=%d top=%d bottom=%d\n", 
             my_rank, coords[0], coords[1],sum, left, right, top, bottom);

    MPI_Finalize();

}


