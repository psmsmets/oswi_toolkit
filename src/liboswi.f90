!*****************************************************************************80
!
!                                   L I B O S W I 
!
!  Module:       LIBOSWI
!
!  Programmer:   Pieter S. M. Smets
!                R&DSA depart. of Seismology and Acoustics
!                Koninklijk Nederlands Meteorologisch Instituut (KNMI)
!                De Bilt, The Netherlands
!
!  Date:         May 20, 2016
!
!  Language:     Fortran-90
!
!  Description:  Library to read and write Ocean Surface Wave Interaction files.
!
!
!*****************************************************************************80

Module liboswi

  use io_types
  use time
  use syms
  use math, only: distance, orthographic_projection, CTBIP
  use netcdf
  use netcdf_ext
  use grib_types
  use grib_2dfd
  use libdem
  use omp_lib
  use iso_c_binding, only: C_LOC, C_F_POINTER

  include 'liboswi_def_parameters.f90'
  include 'liboswi_def_types.f90'
  include 'liboswi_def_variables.f90'
  include 'liboswi_def_readwrite.f90'
  include 'liboswi_def_process.f90'

Contains

  include 'liboswi_version.f90'
  include 'liboswi_types.f90'
  include 'liboswi_set_types.f90'
  include 'liboswi_grid_transform_fun.f90'
  include 'liboswi_process.f90'
  include 'liboswi_write.f90'
  include 'liboswi_read.f90'
  include 'liboswi_filename.f90'

End module liboswi
