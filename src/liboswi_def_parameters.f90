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
!  Description:  liboswi parameter definitions
!
!
!*****************************************************************************80
!
!..parameters
!
  integer(int32), parameter, private  :: len_att    = 80_int32
  integer(int32), parameter, private  :: len_att_gl = 2048_int32
  logical       , parameter, private  :: liboswi_verbose_default = .false.
!
  integer(int32), parameter, private  :: missing_int32  = -9999_int32 
  integer(int32), parameter, private  :: missing_int64  = int(missing_int32,int64)
  real(single)  , parameter, private  :: missing_single = real(missing_int32,single)
  real(double)  , parameter, private  :: missing_double = real(missing_int32,double)
!
