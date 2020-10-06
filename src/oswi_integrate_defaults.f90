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
! swwint init
!
  call oswi_init ( oswi )
!
! default settings
!
  prefix            = ''
  suffix            = ''
  overwrite         = .false.
  help              = .false.
  threads           = 1
  max_threads       = omp_get_max_threads()
  parallel          = .false.
  verb              = .false.
  quick             = .false.
  exists            = .false.
  debug             = .false.
  force_regular_ll  = .false.
  in_nc             = .false.
  integrate         = .true.
!
! default values
!
  nofinfs           = 0_int32
