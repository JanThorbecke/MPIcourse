/****************************************************************
 *                                                              *
 * This file has been written as a sample solution to an        *
 * exercise in a course given at the High Performance           *
 * Computing Centre Stuttgart (HLRS).                           *
 * It is made freely available with the understanding that      *
 * every copy of this file must include this header and that    *
 * HLRS take no responsibility for the use of the               *
 * enclosed teaching material.                                  *
 *                                                              *
 * Authors: Rolf Rabenseifner (HLRS)                            *
 *                                                              *
 * Contact: rabenseifner@hlrs.de                                * 
 *                                                              *  
 * Purpose: Check version of the MPI library and include file   *
 *                                                              *
 * Contents: C-Source                                           *
 *                                                              *
 ****************************************************************/


#include <stdio.h>
#include <mpi.h>

int main (int argc, char *argv[])
{
  int lib_version, lib_subversion;

  MPI_Init(&argc, &argv);

  MPI_Get_version(&lib_version, &lib_subversion);
  printf ("Version: Library: %d.%d, mpi.h: %d.%d\n", 
           lib_version, lib_subversion, MPI_VERSION, MPI_SUBVERSION);

  MPI_Finalize();
}
