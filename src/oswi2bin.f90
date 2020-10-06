!****************************
Program oswi2bin 
!****************************
 
  use io_types
  use string_utility
  use file_utility
  use sort
  use time
  use liboswi
!
  implicit none
! 
! declare varibles
!
  type(type_oswi), target :: oswi
  logical         :: verb=.false.,debug=.false.,quick=.false.,split=.false.,compress=.true.
  logical         :: exists, in_nc, set_in_reference
!
  integer(int16)  :: munit=50_int16
  integer(int32)  :: i, f, inf, nofinfs, error
!
  integer(int32), parameter  :: maxinfs    = 999_int32
  integer(int32), parameter  :: infssize   = 256_int32
  integer(int32), parameter  :: charShort  = 64_int32
!
  real(double)  :: f_first=-1_double, f_last=-1._double
!
  character(len=infssize)   :: filename='', filename_grid='', ncvar=''
  character(len=infssize)   :: ncfile, inf_dummy, arg_type, arg_value, arg_value2
  character(len=infssize)   :: f_standard_name, f_long_name
!
  integer(int32),               pointer             :: numberOfPoints
  real(double)  , dimension(:), pointer             :: lat, lon, dat
!
  logical                , dimension(maxinfs)  :: infs_mask
  integer(int32)         , dimension(maxinfs)  :: ix
  integer(int64)         , dimension(maxinfs)  :: infs_epoch
  character(len=infssize), dimension(maxinfs)  :: infs 
  character(len=charShort)                     :: ext
!
! ---
!
! Get oswi input file
!
  include 'oswi2bin_command_args.f90'
  write (inf_dummy,"(i6)") nofinfs
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
        call oswi_read_hdr ( nc=trim(infs(inf)), hdr=oswi%hdr, mask=infs_mask(inf), debug=debug )
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
          call oswi_read_hdr ( nc=trim(infs(inf)), hdr=oswi%hdr, mask=infs_mask(inf), debug=debug ) 
          if (infs_mask(inf)) set_in_reference=.false.
        else
          infs_mask(inf)=oswi_compare_nc_with_type(nc=trim(infs(inf)),hdr=oswi%hdr,relax=.true.)
        end if
      end if
    end do
    if (count(infs_mask).gt.1_int32) then
      call cocktailsortIx8 ( infs_epoch, nofinfs, ix )
      infs(1:nofinfs)      = infs( ix(1:nofinfs) )
      infs_mask(1:nofinfs) = infs_mask( ix(1:nofinfs) )
    end if
  end if
!
! print number of oswi files
!
  if (count(infs_mask).lt.1_int32) stop 'Error : no NetCDF files with oswi data found!'
  if (verb) then
    print "(a)",  ''
    print "(3a)", 'OSWI2BIN v', trim(liboswi_version()), ' - Convert oswi netCDF to binary files'
    print "(a)",  ''
    print "(4x,a,i4)", 'input files            = ', count(infs_mask)
  end if
!
! Set frequency
!
  if (oswi%hdr%swh.or.oswi%hdr%swi) then
    f_long_name     = 'Sea surface wave'
    f_standard_name = 'Sea_surface_wave'
  else
    f_long_name     = 'Sound'
    f_standard_name = 'Sound'
  endif
!
  if (.not.oswi%hdr%integrate) then
    if (debug) print "('> ',a)", 'Set selected oswi frequency range'
    call oswi_set_frequency_range ( oswi%hdr%f, firstFrequency=f_first, &
      lastFrequency=f_last, sound=.not.(oswi%hdr%swi.or.oswi%hdr%swh) &
    )
  end if
!
! set ll output variable
!
  if (len_trim(ncvar).gt.0.and.oswi%hdr%integrate) then
    if (oswi%hdr%fanalysis) then
      select case (strlowcase(ncvar))
        case ('f','fd','f_dom','frequency','dominant_frequency')
          ncvar='f_dom'
          f_long_name=trim(f_long_name)//' dominant frequency'
        case ('fc','f_cen','center_frequency')
          ncvar='f_cen'
          f_long_name=trim(f_long_name)//' center frequency'
        case ('fb','f_bw','bw','bandwidth')
          ncvar='f_bw'
          f_long_name=trim(f_long_name)//' spectral bandwidth'
        case ('fr','frms','f_rms','rms','rms_frequency')
          ncvar='f_rms'
          f_long_name=trim(f_long_name)//' rms frequency'
        case default
          ncvar=''
      end select
    else
      select case (strlowcase(ncvar))
        case ('f','frq','fd','freq','frequency','dominant_frequency')
          ncvar='f_dom'
        case default
          ncvar=''
      end select
    end if
  else
    ncvar=''
  end if
  if (verb) then
    if (len_trim(ncvar).eq.0) then
      print "(4x,2a)"  , 'NetCDF variable name   = ', trim(oswi%hdr%name)
      print "(4x,2a)"  , 'units                  = ', trim(oswi%hdr%units)
      print "(4x,2a)"  , 'long name              = ', trim(oswi%hdr%long_name)
    else
      print "(4x,2a)"  , 'NetCDF variable name   = ', trim(ncvar)
      print "(4x,2a)"  , 'units                  = ', 'Hz'
      print "(4x,2a)"  , 'long name              = ', trim(f_long_name)
    end if
    print "(4x,2a)", 'gridType               = ', trim(oswi%hdr%grid%gridType)
  end if
!
  select case (oswi%hdr%grid%gridType)
    case ('icosahedron')
      lat=>oswi%hdr%grid%lat%dat
      lon=>oswi%hdr%grid%lon%dat
    case ('regular_ll')
      lat=>oswi%hdr%grid%lat%dat_unpacked
      lon=>oswi%hdr%grid%lon%dat_unpacked
    case ('reduced_ll')
      lat=>oswi%hdr%grid%lat%dat_unpacked
      lon=>oswi%hdr%grid%lon%dat
  end select
!
  numberOfPoints=>oswi%hdr%grid%numberOfPoints
!
  call liboswi_allocate_type_dat( oswi%hdr, oswi%dat, .true. )
  if (oswi%hdr%integrate) then
    select case (ncvar)
      case ('f_cen')
        dat=>oswi%dat%ll%fc
      case ('f_dom')
        dat=>oswi%dat%ll%fd
      case ('f_rms')
        dat=>oswi%dat%ll%fr
      case ('f_bw')
        dat=>oswi%dat%ll%fb
      case default
        dat=>oswi%dat%ll%dat
    end select
  end if
!
! Loop over files
!
  if (len_trim(filename).eq.0) then
    if (split.and..not.oswi%hdr%integrate) then
      filename_grid= '%V__%C_%T__%G__'
      filename= '%V__%C_%T__%G__%FHz__%Y%M%D_%H'
    else
      filename= '%V__%C_%T__%G__%Y%M%D_%H'
    end if
  end if
!
! Write grid
!
  if (split) then
    call liboswi_substitute_filename( filename_grid, ncfile, oswi%hdr, oswi%dat%epoch )
!   lat
    call makefile ( file=trim(ncfile)//'lat.bin', unit=munit, overwrite=.true., &
      format='unformatted' )
    write(munit) real(lat,single)
    close(munit)
!   lon
    call makefile ( file=trim(ncfile)//'lon.bin', unit=munit, overwrite=.true., &
      format='unformatted' )
    write(munit) real(lon,single)
    close(munit)
  end if
!
  do inf=1_int32, nofinfs
!
!   skip?
!
    if (.not.infs_mask(inf)) cycle
!
!   set epoch
!
    oswi%dat%epoch=infs_epoch(inf)
    call epoch2str( oswi%dat%epoch, oswi%dat%time )
    if (verb) print "(4x,2a)", 'date time              = ', trim(oswi%dat%time)
!
!   read data
!
    call oswi_read_nc_data ( &
      nc     = trim(infs(inf)), &
      dat    = oswi%dat, &
      mask   = infs_mask(inf), & 
      debug  = debug &
    )
    if (.not.infs_mask(inf)) cycle
!
    call oswi_modify_output( oswi, debug )
    call oswi_mask_output( oswi, debug )
!
!   compress
!
    if (compress) then
      if (oswi%hdr%integrate) then
        if (len_trim(ncvar).eq.0) then
          where (oswi%dat%ll%mask)
            oswi%dat%ll%dat=log10(oswi%dat%ll%dat)
          elsewhere
            oswi%dat%ll%dat=real(oswi%hdr%missingValue,double)
          end where
        end if
      else
        where (oswi%dat%llf%mask)
          oswi%dat%llf%dat=log10(oswi%dat%llf%dat)
        elsewhere
          oswi%dat%llf%dat=real(oswi%hdr%missingValue,double)
        end where
      end if
    end if
!
!   export
!
    if (split.and..not.oswi%hdr%integrate) then
      if (debug) print "('> ',a)", 'Write to separate binary files'
!
      do i=1_int32,oswi%hdr%f%size
        if (.not.oswi%hdr%f%mask(i)) cycle
        oswi%hdr%f%ixSel=i
        call liboswi_substitute_filename( filename, ncfile, oswi%hdr, oswi%dat%epoch )
        if (debug) print "(2x,'>> ',2a)", 'Write ', trim(ncfile)//'.bin'
        call makefile ( file=trim(ncfile)//'.bin', unit=munit, overwrite=.true., &
          format='unformatted' )
        write(munit) real(oswi%dat%llf%dat(:,i),single)
        close(munit)
      end do
!
    else
      if (debug) print "('> ',a)", 'Write to binary file'
      call liboswi_substitute_filename( filename, ncfile, oswi%hdr, oswi%dat%epoch )
print *, trim(ncfile)
    end if
!
  end do
!

Contains

  include 'oswi2bin_help.f90'

End program oswi2bin
