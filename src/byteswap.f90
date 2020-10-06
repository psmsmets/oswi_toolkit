!*****************************************************************************************************************************
!
!                                                        B Y T E S W A P
!
!  Module:       BYTESWAP
!
!  Programmer:   Pieter S. M. Smets
!                Seismology Devision - Koninklijk Nederlands Meteorologisch Instituut (KNMI)
!                De Bilt, The Netherlands
!
!  Date:         August 29, 2014
!
!  Language:     Fortran-90
!
!  Description:  This module includes several subroutines to reverse the byte ordering, converting
!                little-endian values to big-endian (and vice versa). This is useful when reading
!                binary data from a file intended for use on a compute whose byte order is opposite
!                that of the computer on which the Fortran program is to be run.
!
!                   byte_swap_int8       Swap bytes of a 1-byte integer
!                   byte_swap_int16      Swap bytes of a 2-byte integer
!                   byte_swap_int32      Swap bytes of a 4-byte integer
!                   byte_swap_int64      Swap bytes of a 8-byte integer
!                   byte_swap_float32    Swap bytes of a 4-byte real
!                   byte_swap_float64    Swap bytes of a 8-byte real
!
!*****************************************************************************************************************************

Module byteswap

   use io_types

Contains


! =========================================================================================
!
!.......Subroutine swap_bytes_int8
!
! This subroutines does nothing, as byte swapping for a 8-bit integer cannot occur.
! For safety, to not forget, this subroutine exists. 
!
!****************************
   Subroutine swap_bytes_int8 ( i1 )
!****************************
!
      implicit none
!
!.....Dummy variables
!
      integer( int8 ), intent ( inout )  :: i1
!
! ----
!
      return
!
   End subroutine swap_bytes_int8
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine swap_bytes_int16
!
! This subroutines performs byte swapping of 16-bit integers.
! After calling this subroutine, the input integer will be replaced by the output integer.
!
!****************************
   Subroutine swap_bytes_int16 ( i2 )
!****************************
!
      implicit none
!
!.....Dummy variables
!
      integer( int16 ), intent ( inout )  :: i2 
!
!.....Local variables
!
      integer( int8 )                      :: i
      integer( kind = 1 ), dimension( 2 )  :: byte_arr, byte_arr_tmp
!
! ----
!
      byte_arr = transfer( i2, byte_arr )
      byte_arr_tmp = byte_arr
!
      do i = 1, 2
         byte_arr( i ) = byte_arr_tmp( 3 - i )
      end do
!
      i2 = transfer( byte_arr, i2 )
!
      return
!
   End subroutine swap_bytes_int16
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine swap_bytes_int32
!
! This subroutines performs byte swapping of 32-bit integers.
! After calling this subroutine, the input integer will be replaced by the output integer.
!
!****************************
   Subroutine swap_bytes_int32 ( i4 )
!****************************
!
      implicit none
!
!.....Dummy variables
!
      integer( int32 ), intent ( inout )  :: i4
!
!.....Local variables
!
      integer( int8 )                      :: i
      integer( kind = 1 ), dimension( 4 )  :: byte_arr, byte_arr_tmp
!
! ----
!
      byte_arr = transfer( i4, byte_arr )
      byte_arr_tmp = byte_arr
!
      do i = 1, 4
         byte_arr( i ) = byte_arr_tmp( 5 - i )
      end do
!
      i4 = transfer( byte_arr, i4 )
!
      return
!
   End subroutine swap_bytes_int32
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine swap_bytes_int64
!
! This subroutines performs byte swapping of 64-bit integers.
! After calling this subroutine, the input integer will be replaced by the output integer.
!
!****************************
   Subroutine swap_bytes_int64 ( i8 )
!****************************
!
      implicit none
!
!.....Dummy variables
!
      integer( int32 ), intent ( inout )  :: i8 
!
!.....Local variables
!
      integer( int8 )                      :: i
      integer( kind = 1 ), dimension( 8 )  :: byte_arr, byte_arr_tmp
!
! ----
!
      byte_arr = transfer( i8, byte_arr )
      byte_arr_tmp = byte_arr
!
      do i = 1, 8
         byte_arr( i ) = byte_arr_tmp( 9 - i )
      end do
!
      i8 = transfer( byte_arr, int8 )
!
      return
!
   End subroutine swap_bytes_int64
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine swap_bytes_float32
!
! This subroutines performs byte swapping of 32-bit float.
! After calling this subroutine, the input float will be replaced by the output float.
!
!****************************
   Subroutine swap_bytes_float32 ( f4 )
!****************************
!
      implicit none
!
!.....Dummy variables
!
      real( float32 ), intent ( inout )  :: f4
!
!.....Local variables
!
      integer( int8 )                      :: i
      integer( kind = 1 ), dimension( 4 )  :: byte_arr, byte_arr_tmp
!
! ----
!
      byte_arr = transfer( f4, byte_arr )
      byte_arr_tmp = byte_arr

      do i = 1, 4
         byte_arr( i ) = byte_arr_tmp( 5 - i )
      end do

      f4 = transfer( byte_arr, f4 )
!
      return
!
   End subroutine swap_bytes_float32
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine swap_bytes_float64
!
! This subroutines performs byte swapping of 64-bit float.
! After calling this subroutine, the input float will be replaced by the output float.
!
!****************************
   Subroutine swap_bytes_float64 ( f8 )
!****************************
!
      implicit none
!
!.....Dummy variables
!
      real( float64 ), intent ( inout )  :: f8
!
!.....Local variables
!
      integer( int8 )                      :: i
      integer( kind = 1 ), dimension( 8 )  :: byte_arr, byte_arr_tmp
!
! ----
!
      byte_arr = transfer( f8, byte_arr )
      byte_arr_tmp = byte_arr

      do i = 1, 8
         byte_arr( i ) = byte_arr_tmp( 9 - i )
      end do

      f8 = transfer( byte_arr, f8 )
!
      return
!
   End subroutine swap_bytes_float64
!
! =========================================================================================

End module byteswap
