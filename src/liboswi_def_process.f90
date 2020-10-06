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
!  Description:  liboswi types definitions include
!
!
!*****************************************************************************80
!
  real(double), dimension(:)  , allocatable, target  :: oswi_mod_coeff_f
  real(double), dimension(:,:), allocatable, target  :: oswi_mod_coeff_fd

  integer(int32)                    , parameter         :: nc_x = 1500_int32
  integer(int32)                    , parameter         :: nc_n = 7_int32
  real(single), dimension(nc_x)     , private           :: c_x
  real(single), dimension(nc_n,nc_x), private, target   :: c_n

  include 'Rayleigh_source_c_1500_3000.f90'
