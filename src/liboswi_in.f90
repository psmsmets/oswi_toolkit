!*****************************************************************************80
!
!                                 L I B O S W I
!
!  Module:       LIBOSWI (include)
!
!  Programmer:   Pieter S. M. Smets
!              R&DSA depart. of Seismology and Acoustics
!              Koninklijk Nederlands Meteorologisch Instituut (KNMI)
!              De Bilt, The Netherlands
!
!  Date:         May 20, 2016
!
!  Language:     Fortran-90
!
!  Description:  liboswi examine input list 
!
!
!*****************************************************************************80
!
!..examine input files list
!
  if (in_grib) then
    include 'liboswi_in_grib.f90'
  else
    include 'liboswi_in_nc.f90'
  end if
