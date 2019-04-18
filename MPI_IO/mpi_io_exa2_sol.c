
/************************************************************************
 *
 * This file has been written as a sample solution to an exercise in a 
 * course given at the HLRS www.hlrs.de . It is made
 * freely available with the understanding that every copy of this file
 * must include this header and that HLRS takes no responsibility for
 * the use of the enclosed teaching material.
 *
 * Authors:    Rolf Rabenseifner
 *
 * Contact:    rabenseifner@hlrs.de 
 *
 * Purpose:    A program to test parallel file I/O with MPI.
 *
 * Contents:   C source code.
 *
 ***********************************************************************/
 
#include	<stdio.h>
#include	<mpi.h>

void main (int argc, char *argv[])
{
int my_rank, size, i;
 
int ndims, array_of_sizes[1], array_of_subsizes[1];
int array_of_starts[1], order;
MPI_File fh;
MPI_Datatype etype;
MPI_Datatype filetype;
MPI_Offset disp;
MPI_Status status;

char buf; 

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    etype = MPI_CHAR;
    ndims = 1;
    array_of_sizes[0]    = size;
    array_of_subsizes[0] = 1;
    array_of_starts[0]   = my_rank;
    order = MPI_ORDER_C;
    MPI_Type_create_subarray(ndims, array_of_sizes,
                             array_of_subsizes, array_of_starts,
                             order, etype, &filetype);
    MPI_Type_commit(&filetype);

    MPI_File_open(MPI_COMM_WORLD, "my_test_file",
                  MPI_MODE_RDWR | MPI_MODE_CREATE,
                  MPI_INFO_NULL, &fh);

    disp = 0;
    MPI_File_set_view(fh, disp, etype, filetype, "native",
                      MPI_INFO_NULL);

    for (i=0; i<3; i++) {
      buf = 'a' + (char)my_rank;
      MPI_File_write(fh, &buf, 1, etype, &status);
    }

    MPI_File_close(&fh);

    printf ("PE%d\n", my_rank);

    MPI_Finalize();

}
