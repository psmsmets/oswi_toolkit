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
!  Description:  liboswi export to oswi nc file 
!
!
!*****************************************************************************80
!
! regrid? and export
!
  if (grid_convert) then
    call oswi_copy_dat_essentials( oswi%dat, oswi_dummy%dat, debug )
    if (grid_regular_ll) then
      if (oswi%hdr%integrate) then
        call transform_oswi_reduced_to_regular_ll ( &
          reduced_ll = oswi, & 
          regular_ll = oswi_dummy, & 
          verbose    = verbmore &
          )
      else
        call transform_oswi_reduced_spectrum_to_regular_ll ( &
          reduced_ll = oswi, & 
          regular_ll = oswi_dummy, & 
          verbose    = verbmore &
          )
      end if
    elseif (grid_icosahedron) then
      if (oswi%hdr%integrate) then
        call transform_oswi_reduced_to_icosahedron ( &
          reduced_ll  = oswi, & 
          icosahedron = oswi_dummy, & 
          verbose     = verbmore &
          )
      else
        call transform_oswi_reduced_spectrum_to_icosahedron ( &
          reduced_ll  = oswi, & 
          icosahedron = oswi_dummy, & 
          verbose     = verbmore &
          )
      end if
    end if
    call oswi_write_nc( &
      file        = trim(ncfile), &
      oswi        = oswi_dummy, &
      compression = .true., &
      verbose     = verbmore, &
      verboseAll  = debug &
      )
  else
    call oswi_write_nc( &
      file        = trim(ncfile), &
      oswi        = oswi , &
      compression = .true., &
      verbose     = verbmore, &
      verboseAll  = debug &
      )
  end if
