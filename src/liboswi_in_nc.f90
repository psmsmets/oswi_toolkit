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
!  Description:  liboswi examine input list of oswi nc files
!
!
!*****************************************************************************80
!
!..examine input list of oswi nc files
!
  if (verb) print "('> ',3a)", 'Examine oswi nc input files (#',trim(adjustl(inf_dummy)),')'
  infs_mask=.false.
  set_in_reference=.true.
  if (quick.or.oswi%hdr%debug) then
    do inf=1_int32,nofinfs
      call oswi_read_nc_epoch( nc=trim(infs(inf)), epoch=infs_epoch(inf), mask=infs_mask(inf), verbose=debug )
      if (set_in_reference.and.infs_mask(inf)) then
        call oswi_read_hdr ( nc=trim(infs(inf)), hdr=oswi_dummy%hdr, mask=infs_mask(inf), debug=debug )
        if (infs_mask(inf)) then
          set_in_reference=.false.
          if (oswi%hdr%debug) exit
        end if
      end if
    end do
  else
    do inf=1_int32,nofinfs
      call oswi_read_nc_epoch( nc=trim(infs(inf)), epoch=infs_epoch(inf), mask=infs_mask(inf), verbose=debug )
      if (infs_mask(inf)) then
        if (set_in_reference) then
          call oswi_read_hdr ( nc=trim(infs(inf)), hdr=oswi_dummy%hdr, mask=infs_mask(inf), debug=debug ) 
          if (infs_mask(inf)) set_in_reference=.false.
        else
          infs_mask(inf)=oswi_compare_nc_with_type(nc=trim(infs(inf)),hdr=oswi_dummy%hdr,relax=.true.)
        end if
      end if
    end do
    if (count(infs_mask).gt.1_int32) then
      call cocktailsortIx8 ( infs_epoch, nofinfs, ix )
      infs(1:nofinfs)      = infs( ix(1:nofinfs) )
      infs_mask(1:nofinfs) = infs_mask( ix(1:nofinfs) )
    end if
  end if
! print number of grib files
  if (count(infs_mask).lt.1_int32) stop 'Error : no NetCDF files with oswi data found!'
  if (verb) then
    print "(4x,a,i4)", '# input files          = ', count(infs_mask)
    print "(4x,2a)"  , 'NetCDF variable name   = ', trim(oswi_dummy%hdr%name)
    print "(4x,2a)"  , 'units                  = ', trim(oswi_dummy%hdr%units)
    print "(4x,2a)"  , 'long name              = ', trim(oswi_dummy%hdr%long_name)
  end if
! check if input is ok
  if (oswi_dummy%hdr%type.eq.'swh'.and.oswi%hdr%type.ne.'swh') then
    print "(a)", 'Error: oswi output type requires swi input data!'
    stop 1 
  end if
! set frequency range
  if (debug) print "('> ',a)", 'Set selected oswi frequency range'
  call oswi_set_frequency_range ( &
    f              = oswi_dummy%hdr%f, & 
    firstFrequency = oswi%hdr%f%first, &
    lastFrequency  = oswi%hdr%f%last, &
    sound          = .not.(oswi%hdr%swh.or.oswi%hdr%swi) &
  )
! set oswi
  if (debug) print "('> ',a)", 'Copy essential info from primary NetCDF file to oswi structure'
  call oswi_copy_hdr_essentials ( oswi_dummy%hdr, oswi%hdr, append_history=.true. )
  integrate=.not.oswi_dummy%hdr%integrate
  modulate=integrate.and.oswi_dummy%hdr%swi
! allocate
  call liboswi_allocate_type_dat( oswi%hdr, oswi%dat )
  call liboswi_allocate_type_dat( oswi_dummy%hdr, oswi%dat )
! clear
  call oswi_clear(oswi_dummy)
