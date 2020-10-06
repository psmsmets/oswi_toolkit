!*****************************************************************************80
!
!                               L I B O S W I
!
!  Module:       LIBOSWI (include)
!
!  Programmer:   Pieter S. M. Smets
!            R&DSA depart. of Seismology and Acoustics
!            Koninklijk Nederlands Meteorologisch Instituut (KNMI)
!            De Bilt, The Netherlands
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
! skip?
!
  if (.not.infs_mask(inf)) cycle
!
! set epoch
!
  oswi%dat%epoch=infs_epoch(inf)
!
! set filename
!
  if (grid_convert) then
    call liboswi_substitute_filename( filename, ncfile, oswi_dummy%hdr, oswi%dat%epoch )
  else
    call liboswi_substitute_filename( filename, ncfile, oswi%hdr, oswi%dat%epoch )
  end if
  ncfile = trim(ncfile)//'.nc'
!
  if (verbmore.and..not.debug) then
    print "(4x,2a)", 'NetCDF file            = ', trim(ncfile)
  else
    if (verb) print "(4x,2(a,i4),3a)",  'NetCDF file ', inf, ' of ', nofinfs, &
      ' ( ', trim(ncfile), ' )' 
  end if
!
! Skip file?
!
  inquire( file=trim(ncfile), exist=exists )
  if ( exists.and..not.overwrite ) then
    if (verb) print "(4x,3a)", 'Warning: "', trim(ncfile), &
      '" already exists and overwriting is off. File skipped.'
    cycle 
  end if
