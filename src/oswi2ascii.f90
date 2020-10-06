!****************************
Program oswi2ascii 
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
  logical         :: verb=.false.,debug=.false.,plain=.false.,dosort=.false.
  logical         :: exists, in_nc
  integer(int32)  :: i, f, inf, nofinfs, error
!
  integer(int32), parameter  :: maxinfs    = 1_int32
  integer(int32), parameter  :: infssize   = 256_int32
  integer(int32), parameter  :: charShort  = 64_int32
!
  character(len=infssize)   :: filename, ncvar=''
  character(len=infssize)   :: inf_dummy, arg_type, arg_value, arg_value2
  character(len=infssize)   :: f_standard_name, f_long_name
!
  real(double)  :: f_first=-1_double, f_last=-1._double
!
  integer(int32), dimension(:), allocatable, target :: ix
  integer(int32),               pointer             :: numberOfPoints
  integer(int32), dimension(:), pointer             :: ix_rev
  real(double)  , dimension(:), pointer             :: lat, lon, dat
!
!  logical                , dimension(maxinfs)  :: infs_mask
!  integer(int32)         , dimension(maxinfs)  :: ix
!  integer(int64)         , dimension(maxinfs)  :: infs_epoch
  character(len=infssize), dimension(maxinfs)  :: infs 
  character(len=charShort)                     :: ext
!
! ---
!
! Get oswi input file
!
  include 'oswi2ascii_command_args.f90'
!
  call oswi_read ( nc=trim(infs(1)), oswi=oswi, verbose=debug, debug=debug )
!
! Print header
!
  if (.not.plain) then
    write (output_unit,"(a)")  '#'
    write (output_unit,"(3a)") '# OSWI2ASCII v', trim(liboswi_version()), ' - Convert oswi netCDF to ascii'
    write (output_unit,"(a)")  '#'
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
    if (debug) write (output_unit,"('# ',3a)") &
      'Set selected ', strlowcase(trim(f_long_name)),' frequency range'
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
!
  if (.not.plain) then
    if (len_trim(ncvar).eq.0) then
      write (output_unit,"(2a)") '# NetCDF variable name   = ', trim(oswi%hdr%name)
      write (output_unit,"(2a)") '# units                  = ', trim(oswi%hdr%units)
      write (output_unit,"(2a)") '# long name              = ', trim(oswi%hdr%long_name)
    else
      write (output_unit,"(2a)") '# NetCDF variable name   = ', trim(ncvar)
      write (output_unit,"(2a)") '# units                  = ', 'Hz'
      write (output_unit,"(2a)") '# long name              = ', trim(f_long_name)
    end if
    write (output_unit,"(2a)") '# date time              = ', trim(oswi%dat%time)
    write (output_unit,"(2a)") '# gridType               = ', trim(oswi%hdr%grid%gridType)
  end if
!
  call oswi_modify_output( oswi, debug )
  call oswi_mask_output( oswi, debug )
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
  if (oswi%hdr%integrate) then
    select case (ncvar)
      case ('fc')
        dat=>oswi%dat%ll%fc
      case ('fd')
        dat=>oswi%dat%ll%fd
      case ('fr')
        dat=>oswi%dat%ll%fr
      case ('fb')
        dat=>oswi%dat%ll%fb
      case default
        dat=>oswi%dat%ll%dat
    end select
    if (dosort) then
      allocate(ix(numberOfPoints))
      call cocktailSortIx_dble(dat,numberOfPoints,ix)
      ix_rev=>ix(numberOfPoints:1_int32:-1_int32)
      do i=1_int32,numberOfPoints
        if (.not.oswi%dat%ll%mask(ix_rev(i))) cycle
        write (output_unit,"(2(f11.6,1x),e12.5)") lat(ix_rev(i)), lon(ix_rev(i)), dat(ix_rev(i))
      enddo
      deallocate(ix); nullify(ix_rev)
    else
      do i=1_int32,numberOfPoints
        if (.not.oswi%dat%ll%mask(i)) cycle
      write (output_unit,"(2(f11.6,1x),e12.5)") lat(i), lon(i), dat(i)
      enddo
    endif
  else
    do f=1_int32,oswi%hdr%f%size
      if (.not.oswi%hdr%f%mask(f)) cycle
      do i=1_int32,numberOfPoints
        if (.not.oswi%dat%llf%mask(i,f)) cycle
        write (output_unit,"(2(f11.6,1x),f8.6,1x,e12.5)") &
          lat(i), lon(i), oswi%hdr%f%dat(f), oswi%dat%llf%dat(i,f)
      enddo
    enddo
  end if

Contains

  include 'oswi2ascii_help.f90'

End program oswi2ascii
