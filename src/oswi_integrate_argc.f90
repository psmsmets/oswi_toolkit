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
!  Description:  oswi_integrate command line argument check
!
!
!*****************************************************************************80
!
! oswi command line arguments 
!
  i = 0_int32
  oswi%history = 'oswi'
!
  do while ( i .lt. iargc() )
!
!   read argument type and change to lower case
!
    i = i + 1_int32
    call getarg( i, arg_type )
!
    arg_value  = ''
    arg_value2 = ''
    add_to_history = .true.
!
!   overwrite default values
!
    select case ( strlowcase(arg_type) )
!
!     Debug options
!
      case ('--debug') ! verbose all
        debug=.true.
!
!     set frequency range
!
      case ('-f','--frequency')
        arg_type='-f'
        i=i+1_int32; call getarg( i, arg_value )
        read ( arg_value, *, iostat = io ) oswi%f%first
        if ( io.ne.0 .or.( oswi%f%first.lt.0 .and. nint(oswi%f%first,int32).ne.-1_int32 ) ) &
          & stop 'SWWINT -F illegal lower frequency!'
        i=i+1_int32; call getarg( i, arg_value2 )
        read ( arg_value2, *, iostat = io ) oswi%f%last
        if ( io.ne.0 .or.( oswi%f%last.lt.0 .and. nint(oswi%f%last,int32).ne.-1_int32 ) ) &
          & stop 'SWWINT -F illegal upper frequency!'
        if ( oswi%f%last.lt.oswi%f%first ) stop &
          & 'SWWINT -F illegal input: fmin > fmax'
!
!     help
!
      case ('-?','--help')
        helpfull = .true.
!
!     set output dB
!
      case ('-db','--db')
        arg_type='-db'
        oswi%dB=.true.
        oswi%lg=.false.
!
!     set output Pa
!
      case ('-pa','--pa')
        arg_type='-pa'
        oswi%dB=.false.
        oswi%lg=.false.
!
!     normalize output
!
      case ('-norm','--normalize')
        arg_type='-norm'
        oswi%normalize=.true.
!
!     set variance output
!
      case ('-var','--variance')
        arg_type='-var'
        oswi%variance=.true.
!
!     forece regular_ll output grid
!
      case ('-r','--regular','--regular_ll','--regular-ll')
        arg_type='-r'
        force_regular_ll=.true.
!
!     overwrite netcdf output files
!
      case ('-o','--overwrite')
        arg_type='-o'
        overwrite = .true.
!
!     output prefix
!
      case ('-p','--prefix')
        arg_type='-p'
        i = i + 1; call getarg( i, arg_value )
        prefix=arg_value
!
!     output suffix
!
      case ('-u','--suffix')
        arg_type='-u'
        i = i + 1; call getarg( i, arg_value )
        suffix=arg_value
!
!     quick and dirty
!
      case ('-q','--quick')
        arg_type='-q'
        quick = .true.
!
!     verbose and level
!
      case ('-v','--verbose')
        arg_type='-v'
        verb = .true.
      case ('-n','--silent','--disable-verbose')
        arg_type='-n'
        verb = .false.
!
!     openmp threads
!
      case ('-t','--threads')
        arg_type='-t'
        parallel = .true.
        call getarg( i + 1_int32, arg_value )
        if ( arg_value(1:1).eq.'-' .or. len_trim(arg_value).eq.0 ) then
          threads = max_threads
          arg_value=''
        else
          i = i + 1_int32
          read ( arg_value ,"(i2)", iostat = io ) threads
          if ( io.ne.0 .or. threads.lt.-1 .or. threads.eq.0 .or. threads.gt.max_threads ) then
            threads = max_threads
            print "(3a,i2,a)", 'WARNING: defined threads for',&
              ' openmp (-t) is out of range.', &
              ' Number of threads is put to max_threads = ', &
              threads, '!'
          end if
        end if
!
!     otherwise put in input file/folder list
!
      case default
!
        add_to_history = .false.
!
        if ( nofinfs.le.maxinfs ) then
          if ( len_trim(arg_type).eq.infssize ) then
            print "(2a,i4,4a)", 'WARNING: maximum argument', &
              ' character length of ', infssize, ' reached.', &
              ' File skipped.'
            exit
          end if
          ext=strlowcase(arg_type(scan(trim(arg_type),'.',.true.)+1_int32:len_trim(arg_type)))
          if (ext.ne.'nc') cycle
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
          print "(a,i4,a)", 'WARNING: maximum of ', maxinfs, &
            & ' input file/folder arguments reached!'
        end if
!
    end select
!
    if ( add_to_history ) then
      oswi%history = trim(oswi%history)//' '//trim(arg_type)
      if (len_trim(arg_value) .gt.0) oswi%history = &
        & trim(oswi%history)//' '//trim(arg_value)
      if (len_trim(arg_value2).gt.0) oswi%history = &
        & trim(oswi%history)//' '//trim(arg_value2)
    end if
!
  end do
!
! Overrule
!
  if (oswi%debug) verb=.true.
!

