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
!  Description:  liboswi debug bathymetry or modulation coefficients 
!
!
!*****************************************************************************80
!
! debug bathymetry or modulation coefficients?
!
  if (oswi%hdr%debug) then
    if (     debug_bathymetry) ncfile='bathymetry'
    if (.not.debug_bathymetry) ncfile='modulation_coefficients_'//oswi%hdr%type
    ncfile=trim(ncfile)//'.nc'
    if (.not.debug_bathymetry) then
      call oswi_integrate_spectrum ( oswi, verbmore )
      call oswi_modify_output ( oswi, verbmore )
    else
      if (.not.grid_convert) oswi%dat%ll%maxValue=maxval(oswi%dat%ll%dat)
      if (     grid_convert) oswi_dummy%dat%ll%maxValue=maxval(oswi%dat%ll%dat)
    end if
    if (oswi%hdr%integrate) then
      if (grid_convert.and.grid_regular_ll) then
        call transform_oswi_reduced_to_regular_ll ( &
          reduced_ll = oswi, & 
          regular_ll = oswi_dummy & 
          )
      end if
    else
      if (grid_convert.and.grid_regular_ll) then
        call transform_oswi_reduced_spectrum_to_regular_ll ( &
          reduced_ll = oswi, & 
          regular_ll = oswi_dummy & 
          )
      end if
    end if
    if (grid_convert.and.grid_regular_ll) then
      call oswi_write_nc( &
        file        = trim(ncfile), &
        oswi      = oswi_dummy, &
        compression = .true., &
        verbose     = verbmore, &
        verboseAll  = .false. &
        )
    else
      call oswi_write_nc( &
        file        = trim(ncfile), &
        oswi      = oswi, &
        compression = .true., &
        verbose     = verbmore, &
        verboseAll  = .false. &
        )
    end if
    nofinfs=0_int32
  end if
