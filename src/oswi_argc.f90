!*****************************************************************************80

!
!  Module:       / (include)
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
!  Description:  oswi command line argument check
!
!
!*****************************************************************************80
!
! oswi command line arguments 
!
  i = 0_int32
  oswi%hdr%history = 'oswi'
!
  do while ( i.lt.command_argument_count() )
!
!........read argument type and change to lower case
!
    i = i + 1_int32
    call get_command_argument( i, arg_type )
!
    arg_value  = ''
    arg_value2 = ''
    add_to_history = .true.
!
!........overwrite default values
!
    select case ( strlowcase(arg_type) )
!
!.....Debug options
!
      case ('--export-bathymetry')
        oswi%hdr%debug=.true.
        debug_bathymetry=.true.
!
      case ('--export-modulation')
        oswi%hdr%debug=.true.
!
      case ('--debug') ! verbose all
        debug=.true.
!
      case ('--version')
        write (output_unit,"(a)") trim(liboswi_version())
        stop
!
!.....set ocean source output
!
      case ('-s','-sea','--sea','--ocean')
        arg_type='-s'
        oswi%hdr%type = 'sea'
!
!.....set atmosphere source output
!
      case ('-a','-air','--air','--atmosphere')
        arg_type='-a'
        oswi%hdr%type = 'air'
! add option to select:
!    - 0: LH50/H63
!    - 1: Breckovskikh		=
!    - 2: Waxler 2006		= 
!    - 3: Waxler 2007		= --atmos --finite-ocean
!
!.....set solid source output
!
      case ('-b','-bed','--bed','--bedrock')
        arg_type='-b'
        oswi%hdr%type = 'bed'
!
!.....set sea floor deformation output
!
      case ('-d','--seafloor-deformation','--bedrock-deformation',&
        '--deformation','--def','-def','--sfd','-sfd')
        arg_type='-d'
        oswi%hdr%type = 'sfd'
!
!.....set swh output
!
      case ('-w','-swh','--swh','--significant-wave-height')
        arg_type='-w'
        oswi%hdr%type = 'swh'
!
!.....set hasselmann output
!
      case ('-h','-swi','--swi','-hass','--hass',&
        '--hasselmann','--surface-wave-interaction')
        arg_type='-h'
        oswi%hdr%type = 'swi'
!
!.....set ocean depth (bathymetry and file)
!
      case ('-fd','--finite-depth')
        arg_type='-fd'
        oswi%hdr%dem%defined=.true.
!
      case ('-id','--infinite-depth','--disable-dem')
        arg_type='-id'
        oswi%hdr%dem%defined=.false.
!
      case ('--dem-path','--dem-file')
        arg_type='--dem-file'
        i=i+1_int32; call get_command_argument( i, arg_value )
        dem%file=arg_value
!
      case ('--dem-method')
        i=i+1_int32; call get_command_argument( i, arg_value )
        oswi%hdr%dem%resampleMethod=arg_value(1:len(oswi%hdr%dem%resampleMethod))
!
!!      case ('--dem-native')
!!        oswi%hdr%dem%useNativeRes=.true.
!
!     integrate output spectrum
!
      case ('-i','--integrate')
        arg_type='-i'
        oswi%hdr%integrate=.true.
!
      case ('-i+','--integrate+')
        arg_type='-i+'
        oswi%hdr%integrate=.true.
        oswi%hdr%fanalysis=.true.
!
!     set frequency range
!
      case ('-f','--frequency')
        arg_type='-f'
!
        i=i+1_int32; call get_command_argument( i, arg_value )
        read ( arg_value, *, iostat = io ) oswi%hdr%f%first
        if ( io.ne.0 .or.( oswi%hdr%f%first.lt.0 .and. nint(oswi%hdr%f%first,int32).ne.-1_int32 ) ) &
          & stop 'OSWI -F illegal lower frequency!'
!
        i=i+1_int32; call get_command_argument( i, arg_value2 )
        read ( arg_value2, *, iostat = io ) oswi%hdr%f%last
        if ( io.ne.0 .or.( oswi%hdr%f%last.lt.0 .and. nint(oswi%hdr%f%last,int32).ne.-1_int32 ) ) &
          & stop 'OSWI -F illegal upper frequency!'
!
        if ( oswi%hdr%f%last.lt.oswi%hdr%f%first .and.dabs(oswi%hdr%f%last+1.d0).gt.1.d-8 ) &
          stop 'OSWI -F illegal input: fmin > fmax'
!
!     help
!
      case ('-?','--help')
        call oswi_show_help()
!
!     mask land by bathymetry
!
!!            case ('-m','--mask')
!!               arg_type='-m'
!!              oswi%hdr%clip=.true. 
!
!.....set output dB
!
      case ('-db','--db')
        arg_type='-db'
        oswi%hdr%dB=.true.
        oswi%hdr%lg=.false.
!
!.....set output Pa
!
      case ('-pa','--pa')
        arg_type='-pa'
        oswi%hdr%dB=.false.
        oswi%hdr%lg=.false.
!
!.....normalize output
!
      case ('-norm','--normalize')
        arg_type='-norm'
        oswi%hdr%normalize=.true.
!
!.....set variance output
!
      case ('-var','--variance')
        arg_type='-var'
        oswi%hdr%variance=.true.
!
!.....set output grid
!
      case ('-gr','--grid-regular')
        arg_type='-Gl'
        grid_regular_ll=.true.
        grid_icosahedron=.false.
!
      case ('-gi','--grid-icosahedron')
        arg_type='-Gi'
        grid_regular_ll=.false.
        grid_icosahedron=.true.
!
!.....overwrite netcdf output files
!
      case ('-o','--overwrite')
        arg_type='-o'
        overwrite = .true.
!
!.....filename
!
      case ('-n','--filename')
        arg_type='-n'
        i = i + 1; call get_command_argument( i, arg_value )
        filename = arg_value
!
!.....quick and dirty
!
      case ('-q','--quick')
        arg_type='-q'
        quick = .true.
!
!.....verbose and level
!
      case ('-v','--verbose')
        arg_type='-v'
        verb = .true.
      case ('--silent')
        arg_type=''
        verb = .false.
!
!.....overrule environment variables
!
      case ('--rho-air')
        i=i+1_int32; call get_command_argument( i, arg_value )
        read ( arg_value, *, iostat = io ) dummy_double
        if ( io.ne.0 .or.( dummy_double.lt.0.5_double ) ) &
          & stop '--rho-air illegal value!'
        call oswi_set_env( oswi%hdr%env, rho_air = dummy_double )
!
      case ('--rho-sea')
        i=i+1_int32; call get_command_argument( i, arg_value )
        read ( arg_value, *, iostat = io )dummy_double 
        if ( io.ne.0 .or.( dummy_double.lt.500.0_double ) ) &
          & stop '--rho-sea illegal value!'
        call oswi_set_env( oswi%hdr%env, rho_sea = dummy_double )
!
      case ('--rho-bedrock')
        i=i+1_int32; call get_command_argument( i, arg_value )
        read ( arg_value, *, iostat = io )dummy_double 
        if ( io.ne.0 .or.( dummy_double.lt.1000.0_double ) ) &
          & stop '--rho-bedrock illegal value!'
        call oswi_set_env( oswi%hdr%env, rho_bed = dummy_double )
!
      case ('--c-air')
        i=i+1_int32; call get_command_argument( i, arg_value )
        read ( arg_value, *, iostat = io )dummy_double 
        if ( io.ne.0 .or.( dummy_double.lt.250.0_double ) ) &
          & stop '--c-air illegal value!'
        call oswi_set_env( oswi%hdr%env, c_air = dummy_double )
!
      case ('--c-sea')
        i=i+1_int32; call get_command_argument( i, arg_value )
        read ( arg_value, *, iostat = io )dummy_double 
        if ( io.ne.0 .or.( dummy_double.lt.1000.0_double ) ) &
          & stop '--c-ocean illegal value!'
        call oswi_set_env( oswi%hdr%env, c_sea = dummy_double )
!
      case ('--c-bedrock')
        i=i+1_int32; call get_command_argument( i, arg_value )
        read ( arg_value, *, iostat = io ) dummy_double
        if ( io.ne.0 .or.( dummy_double.lt.1000.0_double ) ) &
          & stop '--c-bedrock illegal value!'
        call oswi_set_env( oswi%hdr%env, c_bed = dummy_double )
!
!.....openmp threads
!
      case ('-t','--threads')
        arg_type='-t'
        parallel = .true.
        call get_command_argument( i + 1_int32, arg_value )
        if ( arg_value(1:1).eq.'-' .or. len_trim(arg_value).eq.0 ) then
          threads = max_threads
          arg_value=''
        else
          i = i + 1_int32
          read ( arg_value ,"(i2)", iostat = io ) threads
          if ( io.ne.0 .or. threads.lt.-1 .or. threads.eq.0 .or. threads.gt.max_threads ) then
            threads = max_threads
            write (output_unit,"(3a,i2,a)") 'WARNING: defined threads for',&
              ' openmp (-t) is out of range.', &
              ' Number of threads is put to max_threads = ', &
              threads, '!'
          end if
        end if
!
!.....otherwise put in input file/folder list
!
      case default
!
        add_to_history = .false.
!
        if ( nofinfs.le.maxinfs ) then
          if ( len_trim(arg_type).eq.infssize ) then
            write (output_unit,"(2a,i4,4a)") 'WARNING: maximum argument', &
              ' character length of ', infssize, ' reached.', &
              ' File skipped.'
            exit
          end if
          ext=strlowcase(arg_type(scan(trim(arg_type),'.',.true.)+1_int32:len_trim(arg_type)))
          select case (strlowcase(ext))
            case ('grib','grb')
              in_grib=.true.
            case ('nc')
              in_nc=.true.
            case default
              cycle
          end select
          inquire ( file=trim(arg_type), exist=exists )
          if (.not.exists) cycle
          exists=.false.
          do inf=1_int32,nofinfs
            exists=arg_type.eq.infs(inf)
            if (exists) exit
          end do
          if (exists) cycle 
          nofinfs=nofinfs+1_int32
          infs(nofinfs)=arg_type
        else
          write (output_unit,"(a,i4,a)") 'WARNING: maximum of ', maxinfs, &
            & ' input file/folder arguments reached!'
        end if
!
    end select
!
    if ( add_to_history ) then
      oswi%hdr%history = trim(oswi%hdr%history)//' '//trim(arg_type)
      if (len_trim(arg_value) .gt.0) oswi%hdr%history = &
        & trim(oswi%hdr%history)//' '//trim(arg_value)
      if (len_trim(arg_value2).gt.0) oswi%hdr%history = &
        & trim(oswi%hdr%history)//' '//trim(arg_value2)
    end if
!
  end do
!
! Overrule
!
  if (debug_bathymetry) then
    oswi%hdr%type='dem'
    oswi%hdr%dem%defined=.true.
    oswi%hdr%integrate=.true.
  end if
  if (debug) verb=.true.
!
! One of a kind?
!
  if (in_grib.and.in_nc) then
    write (error_unit,"(a)") 'Both grib and netcdf data provided! See oswi --help for more information.'
    stop
  elseif (.not.in_grib.and..not.in_nc) then
    write (error_unit,"(a)") 'No data provided! See oswi --help for more information.'
    stop
  end if
