!*****************************************************************************80
!
!                                   L I B O S W I
!
!  Module:       LIBOSWI (include)
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
!  Description:  liboswi local variable definitions
!
!
!*****************************************************************************80
!
! local variables
!
  integer(int32), dimension(:,:), allocatable, private, target  :: icosa_iconvex
  logical       , dimension(:)  , allocatable, private, target  :: icosa_mask
  real(double)  , dimension(:)  , allocatable, private, target  :: icosa_alpha, icosa_beta
!
