!*************************************************************************************************************************
!
!                                                     F I L E _ U T I L I T Y
!
!  Module:       FILE_UTILITY
!
!  Programmer:   Pieter S. M. Smets
!                Seismology Devision - Koninklijk Nederlands Meteorologisch Instituut (KNMI)
!                De Bilt, The Netherlands
!
!  Date:         February 02, 2015
!
!  Language:     Fortran-90
!
!  Description:  This module includes subroutines to handle files and folders
!
!
!*************************************************************************************************************************

Module file_utility

   use io_types
   implicit none

Contains

! =========================================================================================
!
!.......Subroutine makefile
!
! This subroutine creates an ascii file, and if required a folder
!
!****************************
   Subroutine makefile ( file, unit, overwrite, verbose, iostat, format ) 
!****************************
!
!.....Dummy variables
!
      character ( len = * ), intent ( in    )            :: file
      integer( int16 )     , intent ( inout )            :: unit
      logical              , intent ( in    )            :: overwrite
      integer( int16 )     , intent ( out   ), optional  :: iostat
      integer( int16 )     , intent ( in    ), optional  :: verbose
      character ( len = * ), intent ( in    ), optional  :: format
!
!.....Local variables
!
      integer( int16 ) :: ierr, verb
      integer( int32 ) :: j, io_unit
      logical          :: lexist, lopened
      character        :: io_action*1, format1*20
!
!  ---
!
!.....init
!
      ierr = 0_int16
      verb = 6_int16
      format1 = 'formatted'
!
      if ( present( verbose ) ) verb=verbose
      if ( present( format ) ) format1=format
!
!.....scan for directory
!
      j = scan( file, '/', .true. )
!
!.....make folder?
!
      if ( j .gt. 0 ) call makefolder( path=file(1:j), iostat=ierr )
      if ( ierr .ne. 0_int16 ) return
!
!.....check if file exists
!
      inquire ( file=file, exist=lexist, opened=lopened, number=io_unit, action=io_action, iostat=ierr )
!
!.....file exists and overwrite is off
!
      if ( lexist .and. .not. overwrite ) then
         print "(3A)", 'ERROR: file (', trim(file), ') already exists and overwrite is off.'
         ierr = 1_int16
      else if ( lopened ) then
         print ( "(3A,I3,3A)" ), 'ERROR: file "', trim(file), '" is already opened under unit "', &
            & io_unit, '" with "', io_action, '" access.'
         ierr = 1_int16
      else
!
!........open file (and overwrite)
!
         call open_safe ( &
               file=file, &
               unit=unit, &
               form=format1, &
               access='stream', &
               status='replace', & 
               action='write', &
               iostat=ierr, &
               verbose=verbose &
            )
!
      endif 
!
      if ( present( iostat ) ) iostat=ierr
!
      return
!
   End subroutine makefile
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine makefolder
!
!
!****************************
   Subroutine makefolder ( path, iostat ) 
!****************************
!
!.....Dummy variables
!
      character ( len = * ), intent( in  )            :: path
      integer( int16 )     , intent( out ), optional  :: iostat
!
!.....Local variables
!
      character        :: cmdmssg
      logical          :: lexist
      integer( int32 ) :: exitstat, cmdstat 
!
!  ---
!
!.....scan if folder exists
!
      call execute_command_line( &
              'ls '//trim(path)//' >/dev/null', &
              .true., exitstat, cmdstat, cmdmssg &
           )
      lexist=exitstat.eq.0_int32
!
!.....make directory
!
      if ( .not.lexist ) call execute_command_line( &
               'mkdir '//trim(path), &
               .true., exitstat, cmdstat, cmdmssg &
            )
!
      if ( present(iostat) ) iostat=int(exitstat,int16)
!
      return
!
   End subroutine makefolder
!
! =========================================================================================


! =========================================================================================
!
!.......Function isfolder
!
!
!****************************
   Logical function isfolder ( file1 )
!****************************
!
!.....Dummy variables
!
      character ( len = * ), intent ( in ) :: file1
!
!.....Local variables
!
      character        :: cmdmssg
      integer( int16 )  :: io, unit1 
      integer( int32 )  :: l, exitstat, cmdstat 
!
!  ---
!
!.....initialize
!
      l=len_trim( file1 )
      unit1=999_int16
!
!.....scan if folder exists
!
      call execute_command_line( &
              'ls '//trim(file1)//' >/dev/null', &
              .true., exitstat, cmdstat, cmdmssg &
           )
      isfolder=exitstat.eq.0_int32
!
!.....not existing?
!
      if( .not.isfolder ) return
!
!.....does exist, but check if indeed folder!
!
!.....try to make a file in provided path
!
      if ( file1( l:l ) .eq. '/' ) then
         call open_safe( &
               file = trim( file1 )//'._test_if_folder', &
               iostat=io, &
               unit=unit1, &
               status='new', &
               action='write', &
               form='formatted', &
               access='stream' &
            )
      else
         call open_safe( &
               file = trim( file1 )//'/._test_if_folder', &
               iostat=io, &
               unit=unit1, &
               status='new', &
               action='write', &
               form='formatted', &
               access='stream' &
            )
      end if
!
!.....add something in the file
!
      if ( io.eq.0 ) write( unit1,"(A)", iostat=io ) 'test if folder'
!
!.....delete the file 
!
      if ( io.eq.0 ) call close_safe ( unit=unit1, status='delete',iostat=io ) 
!
!.....directory exists if all successful 
!
      isfolder = io.eq.0 
!
      return
!
   End function isfolder
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine open_safe
!
! This function opens a file safely
!
!****************************
   Subroutine open_safe ( file, unit, form, access, status, action, iostat, opened, verbose )
!****************************
!
      implicit none
!
!.....Dummy variables
!
      character( len = * ), intent( in    )            :: file
      integer( int16 )    , intent( inout ), optional  :: unit, iostat
      integer( int16 )    , intent( in    ), optional  :: verbose
      logical             , intent( out   ), optional  :: opened
      character( len = * ), intent( in    ), optional  :: form, access, status, action
!
!.....Local variables
!
      logical            :: lexist, lopened
      character          :: io_action*1
      character(len=16)  :: form1, access1, status1, action1
      integer( int16 )   :: verbose1, unit1, iostat1, io_unit
!
! ----
!
!.....Initialize parameters (default binary file read only)
!
      verbose1 = 6_int16
      unit1    = 1_int16
      form1    = 'unformatted'
      access1  = 'stream'
      status1  = 'old'
      action1  = 'read'
      iostat1  = 0_int16
!
!.....Overwrite parameters
!     
      if ( present(verbose) ) verbose1=verbose
      if ( present(unit) ) unit1=unit
      if ( present(form) ) form1=form
      if ( present(access) ) access1=access
      if ( present(status) ) status1=status
      if ( present(action) ) action1=action
!
!.....Check if file1 exists or is already opened
!
      inquire ( file = file, exist = lexist, opened = lopened, action = io_action, number = io_unit )
!
      if ( .not. lexist .and. action1.eq.'read' ) then
         write ( verbose1, fmt="(3A)" ) 'ERROR: file "', file, '" does not exist.'
         iostat1 = 1_int16
      end if
!
      if ( lopened ) then
         write ( verbose1, fmt="(3A,I3,3A)" ) 'ERROR: file "', file, '" is already opened under unit "', &
            & io_unit, '" with "', io_action, '" access.'
         iostat1 = 1_int16
      end if
!
      if ( iostat1 .ne. 0_int16 ) then
         if ( present(unit) ) unit=io_unit
         if ( present(opened) ) opened=lopened
         if ( present(iostat) ) iostat=iostat1
         return
      end if
!
!.....Check if unit is already in use
!
      if ( unit1 .le. 0_int16 ) unit1 = 10_int16
!
      inquire ( unit = unit1, opened = lopened )
      if ( lopened ) then
         do while ( lopened )
            unit1 = unit1 + 1_int16
            inquire ( unit = unit1, opened = lopened )
         end do
      end if
!
!.....open file
!
      open ( &
            unit   = unit1, &
            file   = file, &
            form   = form1, &
            access = access1, &
            status = status1, &
            action = action1, &
            iostat = iostat1 &
         )
!
      if ( present(unit) )   unit=unit1
      if ( present(opened) ) opened=.true.
      if ( present(iostat) ) iostat=iostat1
!
      return
!
   End subroutine open_safe
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine close_safe
!
! This function closes a file safely
!
!****************************
   Subroutine close_safe ( file, unit, iostat, status, closed, verbose )
!****************************
!
      implicit none
!
!.....Dummy variables
!
      character( len = * ), intent( inout ), optional  :: file
      integer( int16 )    , intent( inout ), optional  :: unit, iostat
      integer( int16 )    , intent( in    ), optional  :: verbose
      logical             , intent( out   ), optional  :: closed
      character( len = * ), intent( in    ), optional  :: status
!
!.....Local variables
!
      logical            :: lexist, lopened, lclosed
      character          :: file1*999
      integer( int16 )   :: verbose1, unit1, iostat1
!
! ----
!
!.....Initialize
!
      verbose1 = 6_int16
      if ( present(verbose) ) verbose1=verbose
!
!.....Check if file name and/or unit are provided
!
      if ( present(file) .and. .not.present(unit) ) then
         file1=file
         inquire ( file=file, exist=lexist, opened=lopened, number=unit1, iostat=iostat1 )

      else if ( .not.present(file) .and. present(unit) ) then
         unit1=unit
         inquire ( unit=unit , name=file1, exist=lexist, opened=lopened, iostat=iostat1 )

      else if ( present(file) .and. present(unit) ) then
         inquire ( file=file, exist=lexist, opened=lopened, number=unit1, iostat=iostat1 )
         inquire ( unit=unit , name=file1, exist=lexist, opened=lopened, iostat=iostat1 )
         if (unit.ne.unit1 .or. file.ne.file1) then
            iostat1=1_int16
         else
            file1=file
            unit1=unit
            iostat1=0_int16
         end if
      else
         write ( verbose1, fmt="(A)" ) 'ERROR: no file or unit provided.'
         iostat=1_int16
         return
      end if
!
      if ( lexist .and. lopened ) then
         if ( present(status) ) then
            close ( unit=unit, iostat=iostat1, status=status )
         else
            close ( unit=unit, iostat=iostat1 )
         end if
      end if
      lclosed=iostat1.eq.0
!
      if ( present(file) ) file=trim(file1)
      if ( present(unit) ) unit=unit1
      if ( present(closed) ) closed=lclosed
      if ( present(iostat) ) iostat=iostat1
!
      return
!
   End subroutine close_safe
!
! =========================================================================================

End module file_utility
