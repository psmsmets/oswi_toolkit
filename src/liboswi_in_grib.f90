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
!  Description:  liboswi examine input list of ecmwf grib files
!
!
!*****************************************************************************80
!
!..examine input list of ecmwf grib files with 2dfd
!
  if (verb) print "('> ',3a)", 'Examine 2d wave spectra input files (#',trim(adjustl(inf_dummy)),')'
  infs_mask=.false.
  set_in_reference=.true.
  if (debug) print "(4x,a,1l1)", 'Quick check            = ', quick.or.oswi%hdr%debug
  if (quick.or.oswi%hdr%debug) then
    do inf=1_int32,nofinfs
      if (debug) write (*,"(4x,'>> check ',a)",advance='no') trim(infs(inf))
      call grib_referenceTime( gribFile=trim(infs(inf)), epoch=infs_epoch(inf), mask=infs_mask(inf) )
      if (set_in_reference.and.infs_mask(inf)) then
        call grib_2dfd_list ( gribFile=trim(infs(inf)), wam2dfd=ow2dfd, mask=infs_mask(inf) )
        if (infs_mask(inf)) then
          set_in_reference=.false.
          if (oswi%hdr%debug) exit
        end if
      end if
      if (debug) print "(1x,'(',1l1,')')", infs_mask(inf)
    end do
  else
    do inf=1_int32,nofinfs
      if (debug) write (*,"(4x,'>> check ',a)",advance='no') trim(infs(inf))
      call grib_referenceTime( gribFile=trim(infs(inf)), epoch=infs_epoch(inf), mask=infs_mask(inf) )
      if (infs_mask(inf)) then
        if (set_in_reference) then
          call grib_2dfd_list ( gribFile=trim(infs(inf)), wam2dfd=ow2dfd, mask=infs_mask(inf) )
          if (infs_mask(inf)) set_in_reference=.false.
        else
          infs_mask(inf)=grib_2dfd_compare_gribFile_with_type(gribFile=trim(infs(inf)),wam2dfd=ow2dfd)
        end if
      end if
      if (debug) print "(1x,'(',1l1,')')", infs_mask(inf)
    end do
    if (count(infs_mask).gt.1_int32) then
      call cocktailsortIx8 ( infs_epoch, nofinfs, ix )
      infs(1:nofinfs)      = infs( ix(1:nofinfs) )
      infs_mask(1:nofinfs) = infs_mask( ix(1:nofinfs) )
    end if
  end if
! print number of grib files
  if (count(infs_mask).lt.1_int32) stop 'Error : no grib files with 2dfd data found!'
  if (verb) print "(4x,a,i4)",'# input files          = ', count(infs_mask)
! set frequency range
  call grib_2dfd_set_frequency_range ( &
    frequency      = ow2dfd%frequency, & 
    firstFrequency = oswi%hdr%f%first, &
    lastFrequency  = oswi%hdr%f%last, &
    doubled        = .not.(oswi%hdr%swh.or.oswi%hdr%swi) &
  )
! copy grib header to oswi type
  call oswi_set_osw ( oswi, ow2dfd )
!
  if (verb) then
    print "(4x,a,2(f7.3,a),i2,a)", 'wave direction         = ', &
      ow2dfd%direction%dat(1), ' deg to ', &
      ow2dfd%direction%dat(ow2dfd%direction%size), ' deg (#', ow2dfd%direction%size ,')'
    print "(4x,a,2(f8.5,a),i2,a)", 'wave frequency         = ', &
      ow2dfd%frequency%dat(1), ' Hz to ', &
      ow2dfd%frequency%dat(ow2dfd%frequency%size), ' Hz (#', ow2dfd%frequency%size ,')'
    if (ow2dfd%frequency%count.lt.ow2dfd%frequency%size) &
      print "(4x,a,2(f8.5,a),i2,a)", 'selected wave range    = ', &
      oswi%hdr%f%wave(1_int32), ' Hz to ', oswi%hdr%f%wave(oswi%hdr%f%size), ' Hz (#', oswi%hdr%f%size ,')'
  end if
  integrate=.true.
  modulate=.true.

