!*****************************************************************************80
!
!                  O S W I _ P S S P _ C O M M A N D _ A R G S
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
!  Descripterrorn:  oswi_pssp command line argument check
!
!
!*****************************************************************************80
!
! oswi_pssp command line arguments 
!
  if (command_argument_count().eq.0_int32) call oswi_pssp_show_help ()
!
! Get command arguments
!
  do while ( i.lt.command_argument_count() )
!
!   read argument type and change to lower case
!
    i = i + 1_int32
    call get_command_argument( i, arg_type )
    arg_value  = ''
!
!   Overwrite default values
!
    select case ( strlowcase(arg_type) )
!
!.....Help
!
      case ('-?','--help')
        call oswi_pssp_show_help ()
!
!.....Debug options
!
      case ('--debug') ! verbose all
        debug=.true.
!
      case ('--version')
        write (output_unit,"(a)") trim(liboswi_version()) 
        stop
!
!.....set frequency range
!
      case ('-f','--frequency')
        arg_type='-f'
!
        i=i+1_int32; call get_command_argument( i, arg_value )
        read ( arg_value, *, iostat = error ) f_first
        if ( error.ne.0 .or.( f_first.lt.0 .and. nint(f_first,int32).ne.-1_int32 ) ) &
          & stop 'OSWI2SEISM -F illegal lower frequency!'
!
        i=i+1_int32; call get_command_argument( i, arg_value2 )
        read ( arg_value2, *, iostat = error ) f_last
        if ( error.ne.0 .or.( f_last.lt.0 .and. nint(f_last,int32).ne.-1_int32 ) ) &
          & stop 'OSWI2SEISM -F illegal upper frequency!'
!
        if ( f_last.lt.f_first .and.dabs(f_last+1.d0).gt.1.d-8 ) &
          stop 'OSWI2SEISM -F illegal input: fmin > fmax'
!
!.....filename
!
      case ('-n','--filename')
        i = i + 1; call get_command_argument( i, arg_value )
        filename = arg_value
!
!.....Perform quick comparison
!
      case ('-q','--quick')
        quick = .true.
!
!.....Verbose
!
      case ('-v','--verbose')
        verb = .true.
      case ('--silent')
        verb = .false.
!
!.....otherwise put in input file/folder list
!
      case default
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
  end do
!
! Overrule
!
  if (debug) verb=.true.
!
! One of a kind?
!
  if (.not.in_nc) then
    write (error_unit,"(a)") 'No data provided! See oswi_pssp --help for more information.'
    stop
  end if
