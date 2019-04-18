PROGRAM version

!==============================================================!
!                                                              !
! This file has been written as a sample solution to an        !
! exercise in a course given at the High Performance           !
! Computing Centre Stuttgart (HLRS).                           !
! It is made freely available with the understanding that      !
! every copy of this file must include this header and that    !
! HLRS take no responsibility for the use of the               !
! enclosed teaching material.                                  !
!                                                              !
! Authors: Rolf Rabenseifner (HLRS)                            !
!                                                              !
! Contact: rabenseifner@hlrs.de                                !
!                                                              ! 
! Purpose: Check version of the MPI library and include file   !
!                                                              !
! Contents: Fortran-Source                                     !
!                                                              !
!==============================================================!


  USE mpi
  IMPLICIT NONE

  INTEGER lib_version, lib_subversion, lib_version_max, ierror

  CALL MPI_INIT(IERROR=ierror)

  CALL MPI_GET_VERSION(VERSION=lib_version, SUBVERSION=lib_subversion, IERROR=ierror)
  WRITE(*,*) "Version: Library:    ", lib_version, lib_subversion
  WRITE(*,*) "Version: mpi module: ", MPI_VERSION, MPI_SUBVERSION

! ... Only for checking whether keyword based argument lists work with routines that contain a buffer.
! ... Since MPI-3.0, keyword based argument lists are mandatory for the mpi module if it is available!
! ... See MPI-3.0, page 601, line 35.
! ... MPI-3.1, page 609, lines 26-30 allow an restriction, but nearly all compiler
!     (except older versions of gfortran) provide methods to ignore the type of an argument.
  CALL MPI_ALLREDUCE(SENDBUF=lib_version, RECVBUF=lib_version_max, COUNT=1, &
                     DATATYPE=MPI_INTEGER, OP=MPI_MAX, COMM=MPI_COMM_WORLD, IERROR=ierror)

  CALL MPI_FINALIZE(IERROR=ierror)

END
