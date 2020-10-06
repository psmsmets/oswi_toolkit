!****************************
Program oswi2seism 
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
  logical         :: verb=.false.,debug=.false.,quick=.false.
  logical         :: exists, in_nc, set_in_reference
!
  integer(int16)  :: munit=50_int16
  integer(int32)  :: i, f, inf, nofinfs, error
!
  integer(int32), parameter  :: maxinfs    = 999_int32
  integer(int32), parameter  :: infssize   = 256_int32
  integer(int32), parameter  :: charShort  = 64_int32
!
  real(double)  :: f_first=-1_double, f_last=-1._double, icosa_area, f0
  real(double)  , dimension(:), allocatable         :: df 
!
  character(len=infssize)   :: filename='', filename_grid='' 
  character(len=infssize)   :: ncfile, inf_dummy, arg_type, arg_value, arg_value2
!
  integer(int32),               pointer             :: numberOfPoints
  logical       , dimension(:), pointer             :: mask 
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
  include 'oswi2seism_command_args.f90'
  write (inf_dummy,"(i6)") nofinfs
!
! print header
!
  if (verb) then
    write (output_unit,"(a)")  ''
    write (output_unit,"(3a)") 'OSWI2SEISM v', trim(liboswi_version()), &
      ' - Convert oswi bedrock netCDF data to microseism forcing'
    write (output_unit,"(a)")  ''
  end if
!
!..examine input list of oswi nc files
!
  if (verb) write (output_unit,"('> ',3a)") 'Examine oswi nc input files (#',trim(adjustl(inf_dummy)),')'
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
! check
!
  if (count(infs_mask).lt.1_int32.or..not.oswi%hdr%bed.or.oswi%hdr%integrate &
    .or.oswi%hdr%grid%gridType.ne.'icosahedron') &
    stop 'Error : no files with oswi spectral bedrock pressure data found!'
!
! print number of oswi files
!
  if (verb) write (output_unit,"(4x,a,i4)") 'input files             = ', count(infs_mask)
!
! Set frequency
!
  if (debug) write (output_unit,"('> ',a)") 'Set selected oswi frequency range'
  call oswi_set_frequency_range ( oswi%hdr%f, firstFrequency=f_first, &
    lastFrequency=f_last, sound=.true. &
  )
!
! set ll output variable
!
  icosa_area=sqrt(3._double)/4*oswi%hdr%grid%icosahedronEdge**2
!
  if (verb) then
    write (output_unit,"(4x,2a)")     'NetCDF variable name    = ', trim(oswi%hdr%name)
    write (output_unit,"(4x,2a)")     'units                   = ', 'N' 
    write (output_unit,"(4x,2a)")     'long name               = ', trim(oswi%hdr%long_name)
    write (output_unit,"(4x,a,i2)")   'frequencies         (-) = ', oswi%hdr%f%count
    write (output_unit,"(4x,a,f6.4)") 'first frequency    (Hz) = ', ( oswi%hdr%f%dat(oswi%hdr%f%ixFirst) &
      + oswi%hdr%f%dat(oswi%hdr%f%ixFirst+1) ) * .5_double
    write (output_unit,"(4x,a,f6.4)") 'last frequency     (Hz) = ', ( oswi%hdr%f%dat(oswi%hdr%f%ixLast-1) &
      + oswi%hdr%f%dat(oswi%hdr%f%ixLast) ) * .5_double
    write (output_unit,"(4x,2a)")     'gridType                = ', trim(oswi%hdr%grid%gridType)
    write (output_unit,"(4x,a,i8)"  ) 'numberOfPoints      (-) = ', oswi%hdr%grid%numberOfPoints
    write (output_unit,"(4x,a,f8.4)") 'icosahedron edge   (km) = ', oswi%hdr%grid%icosahedronEdge*1e-3
    write (output_unit,"(4x,a,f8.4)") 'icosahedron area (km^2) = ', icosa_area*1e-6
  end if
!
  select case (oswi%hdr%grid%gridType)
    case ('icosahedron')
      lat=>oswi%hdr%grid%lat%dat
      lon=>oswi%hdr%grid%lon%dat
    case default
      stop 'This should not have happened'
  end select
!
! get df
!
  allocate(df(oswi%hdr%f%size))
  f0=0._double
  do f=0_int32,oswi%hdr%f%size-1_int32
    f0 = f0 + oswi%hdr%f%dat(f+1_int32) / 1.1**(f)
  enddo
  f0=f0/oswi%hdr%f%size
!
  do f=0_int32,oswi%hdr%f%size-1_int32
    df(f+1_int32) = f0 * ( 1.1**(f+.5_double) - 1.1**(f-.5_double))
  enddo 
!
  numberOfPoints=>oswi%hdr%grid%numberOfPoints
!
  call liboswi_allocate_type_dat_llf( oswi%hdr, oswi%dat%llf, .true. )
!
! Loop over files
!
  if (len_trim(filename).eq.0) then
    filename_grid= '%V__%C_%T_%R__%G__'
    filename= '%V__%C_%T_%R__%G__%FHz__%E'
  end if
!
! Write grid
!
  if (verb) write (output_unit,"('> ',3a)") 'Write icosahedron grid'
  call liboswi_substitute_filename( filename_grid, ncfile, oswi%hdr, oswi%dat%epoch )
! lat
  if (verb) write (output_unit,"(2x,2a)") '>> latitude grid          = ', trim(ncfile)//'lat.bin'
  call makefile ( file=trim(ncfile)//'lat.bin', unit=munit, overwrite=.true., &
    format='unformatted' )
  write(munit) real(lat,single)
  close(munit)
! lon
  if (verb) write (output_unit,"(2x,2a)") '>> longitude grid         = ', trim(ncfile)//'lon.bin'
  call makefile ( file=trim(ncfile)//'lon.bin', unit=munit, overwrite=.true., &
    format='unformatted' )
  write(munit) real(lon,single)
  close(munit)
!
  if (verb) write (output_unit,"('> ',3a)") 'Write spectral microseism data'
  if (verb) write (output_unit,"(2x,2a)") '>> filename format        = ', trim(filename)
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
    if (verb) write (output_unit,"(4x,2(a,i3),3a)")  'NetCDF file ', inf, ' of ', nofinfs, &
      ' ( ', oswi%dat%time(1:23), ' )'
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
!   export
!
    if (debug) write (output_unit,"('> ',a)") 'Write to binary files'
!
    do f=oswi%hdr%f%ixFirst,oswi%hdr%f%ixLast
!
      if (.not.oswi%hdr%f%mask(f)) cycle
      oswi%hdr%f%ixSel=f
!
      dat=>oswi%dat%llf%dat(:,f)
      mask=>oswi%dat%llf%mask(:,f)
!
      call liboswi_substitute_filename( filename, ncfile, oswi%hdr, oswi%dat%epoch )
      if (debug) write (output_unit,"(2x,'>> ',2a)") 'Write ', trim(ncfile)//'.bin'
!
      where (mask)
        dat=log10( df(f) * dat * icosa_area )
      elsewhere
        dat=real(oswi%hdr%missingValue,double)
      end where
!
      call makefile ( file=trim(ncfile)//'.bin', unit=munit, overwrite=.true., &
        format='unformatted' )
      write(munit) real(dat,single)
      close(munit)
!
    end do
!
  end do
!
  call oswi_clear(oswi)
  deallocate(df)
  nullify(lat,lon,dat,mask)
!

Contains

  include 'oswi2seism_help.f90'

End program oswi2seism
