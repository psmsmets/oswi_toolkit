!*************************************************************************************************************************
!
!                                            N E T C D F _ E X T 
!
!  Module:       NETCDF_EXT
!
!  Programmer:   Pieter S. M. Smets
!                R&D depart. of Seismology and Acoustics - Koninklijk Nederlands Meteorologisch Instituut (KNMI)
!                De Bilt, The Netherlands
!
!  Date:         June 7, 2016
!
!  Language:     Fortran-90
!
!  Description:  Additional commonly used NetCDF functions.
!
!
!*************************************************************************************************************************

Module netcdf_ext

   use io_types
   use netcdf

   implicit none

!
!..initialize verbose
!
   logical, parameter, private  :: verb_init = .true.
!
!..parameters
!
   integer(int8) , parameter, public  :: nf90_true=1_int8, nf90_false=0_int8

Contains

! =========================================================================================
!
!.......Function isnetcdf
!
! Check if file is of netcdf type
!
!****************************
   Logical Function isnc ( file )
!****************************
!
      implicit none
!
!.....Dummy variables
!
      character(len=*), intent( in )  :: file
!
!.....Local variables
!
      integer(int32) :: unit, ierr
!
!  ---
!
!.....Open the netCDF file.
!
      ierr = nf90_open( file, 0, unit )
!
!.....If no error, file is netcdf
!
      isnc = ierr .eq. nf90_noerr
!
!.....If open succesfull, close file again
!
      if ( isnc ) call nc_check( nf90_close ( unit ) )
!
      return
!
   End function isnc
!
! =========================================================================================


! =========================================================================================
!
!.......subroutine nc_check 
!
! Check ierr of NETCDF routines and print error message and terminate program.
!
!****************************
   Subroutine nc_check ( status, verbose, prefix, mask, halt )
!****************************
!
      implicit none
!
!.....Dummy variables
!
      integer(int32)  , intent(in)             :: status
      logical         , intent(in) , optional  :: verbose, halt
      character(len=*), intent(in) , optional  :: prefix
      logical         , intent(out), optional  :: mask
!
!.....Local variables
!
      logical  :: verb, dohalt
!
!  ---
!
      verb=verb_init
      if (present(verbose)) verb=verbose
      if (present(halt)) then
         dohalt=halt
      else
         dohalt=.not.present(mask)
      end if
!
      if ( status.ne.nf90_noerr ) then
         if (verb) then
            if (present(prefix)) then
               print "(a,1x,a)", trim(prefix), trim( adjustl(nf90_strerror(status)) )
            else
               print "(a)", trim( adjustl(nf90_strerror(status)) )
            end if
         end if
         if (dohalt) stop
         if (present(mask)) mask=.false.
      else
         if (present(mask)) mask=.true.
      end if
!
      return
!
   End subroutine nc_check
!
! =========================================================================================

End module netcdf_ext
