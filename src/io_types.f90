!*****************************************************************************80
!
!                                       I O _ T Y P E S
!
!  Module:       IO_TYPES
!
!  Programmer:   Pieter S. M. Smets
!                R&D Seismology and Acoustics,
!                Royal Netherlands Meteorological Institute (KNMI)
!                De Bilt, The Netherlands

!
!  Date:         August 2, 2017
!
!  Language:     Fortran-90
!
!  Description:  This module defines standard integer and floating-point precisions,
!                and functions to convert unsigned integers to signed and vice versa. 
!
!                   uint8_to_int16    convert uint8 to int32
!                   uint16_to_int32   convert uint16 to int32
!                   uint32_to_int64   convert uint32 to int64
!
!*****************************************************************************80

Module io_types

  use, intrinsic :: iso_fortran_env

!
!..signed integer definition
!
! integer, parameter :: int8  = selected_int_kind(  2 )
! integer, parameter :: int16 = selected_int_kind(  4 )
  integer, parameter :: int24 = selected_int_kind(  6 )
! integer, parameter :: int32 = selected_int_kind(  9 )
! integer, parameter :: int64 = selected_int_kind( 18 )
  integer, parameter :: short = int16
  integer, parameter :: long  = int32
!
!..unsigned integer definition
!
  integer, parameter :: uint8  = int8
  integer, parameter :: uint16 = int16
  integer, parameter :: uint32 = int32
  integer, parameter :: uint64 = int64
  integer, parameter :: ushort = int16
  integer, parameter :: ulong  = int32
!
!..floating-point numbers: http://fortranwiki.org/fortran/show/Real+precision
!
  integer, parameter :: half     = selected_real_kind(  3       )
! integer, parameter :: single   = selected_real_kind(  6,   37 )
! integer, parameter :: double   = selected_real_kind( 15,  307 )
! integer, parameter :: quad     = selected_real_kind( 33, 4931 )
  integer, parameter :: single   = real32
  integer, parameter :: double   = real64 
  integer, parameter :: quad     = real128
  integer, parameter :: float32  = single
  integer, parameter :: float64  = double
  integer, parameter :: float128 = quad

Contains

!*****************************************************************************80
!
! Function get_uint8
!
! Convert unsigned integer 8 to signed integer 16
!
!****************************
  Function get_uint8 ( i ) result ( idx )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer( uint8  ), intent ( in )  :: i 
    integer(  int16 )                 :: idx
!
! ----
!
!.....Convert
!
    if ( i .lt. 0 ) then
      idx = int( i + 2._double ** 8, int16 ) 
    else
      idx = int( i, int16 )
    end if
!
    return
!
  End function get_uint8
!
!*****************************************************************************80


!*****************************************************************************80
!
! Function make_uint8
!
! Convert unsigned integer 8 stored in an integer 16 to a signed integer 8.
!
!****************************
  Function make_uint8 ( i ) result ( idx )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer(  int32 ), intent ( in  )  :: i
    integer( uint16 )                  :: idx
!
! ----
!
!.....Convert
!
    if ( i .lt. 0_int16 .or. i .gt. 255_int16 ) then
      stop 'ERROR: unsigned integer 8 should be positive and smaller than 255 (2^8 -1).' 
    else if ( i .gt. 127_int16 ) then
      idx = int( i - 2._double ** 16, int8 )
    else
      idx = int( i, int8 )
    end if
!
    return
!
  End function make_uint8
!
!*****************************************************************************80


!*****************************************************************************80
!
! Function get_uint16
!
! Convert unsigned integer 16 to signed integer 32.
!
!****************************
  Function get_uint16 ( i ) result ( idx )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer( uint16 ), intent ( in  )  :: i
    integer(  int32 )                  :: idx
!
! ----
!
!.....Convert
!
    if ( i .lt. 0 ) then
      idx = int( i + 2._double ** 16, int32 ) 
    else
      idx = int( i, int32 )
    end if
!
    return
!
  End function get_uint16
!
!*****************************************************************************80


!*****************************************************************************80
!
! Function make_uint16
!
! Convert unsigned integer 16 stored in an integer 32 to a signed integer 16.
!
!****************************
  Function make_uint16 ( i ) result ( idx )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer(  int32 ), intent ( in  )  :: i
    integer( uint16 )                  :: idx
!
! ----
!
!.....Convert
!
    if ( i .lt. 0_int32 .or. i .gt. 65535_int32 ) then
      stop 'ERROR: unsigned integer 16 should be positive and smaller than 65535 (2^16 -1).' 
    else if ( i .gt. 32767_int32 ) then
      idx = int( i - 2._double ** 16, int16 )
    else
      idx = int( i, int16 )
    end if
!
    return
!
  End function make_uint16
!
!*****************************************************************************80


!*****************************************************************************80
!
! Function get_uint32
!
! Convert unsigned integer 32 to signed integer 64
!
!****************************
  Function get_uint32 ( i ) result ( idx )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer( uint32 ), intent ( in  )  :: i
    integer(  int64 )                  :: idx
!
! ----
!
!.....Convert
!
    if ( i .lt. 0 ) then
      idx = int( i + 2._double ** 32, int64 ) 
    else
      idx = int( i, int64 )
    end if
!
    return
!
  End function get_uint32
!
!*****************************************************************************80


!*****************************************************************************80
!
! Function get_uint
!
! Convert unsigned integer 32 to signed integer 64
!
!****************************
  Function get_uint ( i, b ) result ( idx )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    real( double ) , intent ( in  )  :: i
    integer( int8 ), intent ( in  )  :: b
    real( double )                   :: idx
!
! ----
!
!.....Convert
!
    if ( i .lt. 0._double ) then
      idx = i + 2._double ** b 
    else
      idx = i
    end if
!
    return
!
  End function get_uint
!
!*****************************************************************************80


!*****************************************************************************80
!
! Function make_uint32
!
! Convert unsigned integer 32 stored in an integer 64 to a signed integer 32.
!
!****************************
  Function make_uint32 ( i ) result ( idx )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer(  int64 ), intent ( in  )  :: i
    integer( uint32 )                  :: idx
!
! ----
!
!.....Convert
!
    if ( i .lt. 0_int64 .or. i .gt. ( 2._double ** 32 - 1._double ) ) then
      stop 'ERROR: unsigned integer 32 should be positive and smaller than 2^32 -1.' 
    else if ( i .gt. ( 2._double ** 31 - 1._double ) ) then
      idx = int( i - 2._double ** 32, int32 )
    else
      idx = int( i, int32 )
    end if
!
    return
!
  End function make_uint32
!
!*****************************************************************************80


!*****************************************************************************80
!
! Function is_little_endian
!
! Check if the machine is little endian.
!
!****************************
  Function is_little_endian ( ) result ( little )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    logical  :: little
!
!.....Local variables
!
    integer, parameter :: ik1 = selected_int_kind( 2 )
    integer, parameter :: ik4 = selected_int_kind( 9 )
!
! ----
!
    little = btest( transfer( int( (/1,0,0,0/), ik1 ),1_ik4 ), 0 ) 
!
    return
!
  End function is_little_endian
!
!*****************************************************************************80


!*****************************************************************************80
!
! Function is_big_endian
!
! Check if the machine is big endian.
!
!****************************
  Function is_big_endian ( ) result ( bigend )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    logical  :: bigend
!
! ----
!
    bigend = .not. is_little_endian()
!
    return
!
  End function is_big_endian
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine get_int24
!
! Convert 24-bit integer ( 3* 8-bit ) to 32-bit integer
!
!****************************
  Subroutine get_int24 ( i8, i32, swapflag )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer( int8  ), dimension( : )         , intent ( in  )  :: i8
    integer( int32 ), dimension( size(i8)/3 ), intent ( out )  :: i32
    logical                                  , intent ( in  )  :: swapflag
!
!.....Local variables
!
    integer( int32 )  :: i
    integer( int8 ), dimension( 3 )  :: order
    integer( int8 ), dimension( 4 )  :: byte_arr
!
! ----
!
!.....Initialize
!
    byte_arr = 0_int8
!
    if ( swapflag ) then
      order = (/ 3_int8, 2_int8, 1_int8 /)
    else
      order = (/ 1_int8, 2_int8, 3_int8 /)
    end if
!
!.....Convert 24-bit integer ( 3* 8-bit ) to 32-bit integer
!
    do i = 1_int32, size( i8 ) / 3
      if ( swapflag ) then
        byte_arr( 2 : 4 ) = i8(  order + 3_int32 * ( i - 1_int32 )  )
      else
        byte_arr( 1 : 3 ) = i8(  order + 3_int32 * ( i - 1_int32 )  )
      end if
      i32( i ) = transfer( byte_arr, 1_int32 )
    end do
!
    return
!
  End subroutine get_int24
!
!*****************************************************************************80


!*****************************************************************************80
!
! Function bitand_int8
!
! This functions returns the bit-wise AND of two nonnegative integer arguments A and B.
!
! Example:
! The five-bit binary representations of the integers 13 and 27 are 01101 and 11011,
! respectively. Performing a bit-wise AND on these numbers yields 01001, or 9.
!
!****************************
  Function bitand_int8 ( a, b ) result ( c )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer( int8 ), intent ( in )  :: a, b
    integer( int8 )                 :: c
!
!.....Local variables
!
    integer( int8 )  :: i
!
! ----
!
!.....Check bits
!
    c = 0_int8
!
    do i = 1_int8, 8_int8
      if ( btest( a, i ) .and. btest( b, i ) ) c = ibset( c, i )
    end do
!
    return
!
  End function bitand_int8
!
!*****************************************************************************80


!*****************************************************************************80
!
! Function bitand_int16
!
! This functions returns the bit-wise AND of two nonnegative integer arguments A and B.
!
! Example:
! The five-bit binary representations of the integers 13 and 27 are 01101 and 11011,
! respectively. Performing a bit-wise AND on these numbers yields 01001, or 9.
!
!****************************
  Function bitand_int16 ( a, b ) result ( c )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer( int16 ), intent ( in )  :: a, b
    integer( int16 )                 :: c
!
!.....Local variables
!
    integer( int8 )  :: i
!
! ----
!
!.....Check bits
!
    c = 0_int16
!
    do i = 1_int8, 16_int8
      if ( btest( a, i ) .and. btest( b, i ) ) c = ibset( c, i )
    end do
!
    return
!
  End function bitand_int16
!
!*****************************************************************************80


!*****************************************************************************80
!
! Function bitand_int32
!
! This functions returns the bit-wise AND of two nonnegative integer arguments A and B.
!
! Example:
! The five-bit binary representations of the integers 13 and 27 are 01101 and 11011,
! respectively. Performing a bit-wise AND on these numbers yields 01001, or 9.
!
!****************************
  Function bitand_int32 ( a, b ) result ( c )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer( int32 ), intent ( in )  :: a, b
    integer( int32 )                 :: c
!
!.....Local variables
!
    integer( int8 )  :: i
!
! ----
!
!.....Check bits
!
    c = 0_int32
!
    do i = 0_int8, 31_int8
      if ( btest( a, i ) .and. btest( b, i ) ) c = ibset( c, i )
    end do
!
    return
!
  End function bitand_int32
!
!*****************************************************************************80


!*****************************************************************************80
!
! Function bitsign
!
!****************************
  Function bitsign ( i, n ) result ( d )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer( int32 ), intent ( in )  :: i, n
    real( double  )                  :: d
!
!.....Local variables
!
    real( double )    :: bt
!
! ----
!
    if (  btest( i , n - 1_int32 )  ) then
      bt = 2._double ** i
    else
      bt = 0._double
    end if
    d = real( iand(  ishft( i, 0_int32 ), 2 ** n - 1 ), kind = double ) - bt
!
    return
!
  End function bitsign
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine bitsplit_int32
!
! This functions splits the B-bit number X into signed N-bit array.
!
!    - X must be unsigned integer class
!    - N ranges from 1 to B
!    - B is a multiple of N
!
!****************************
  Subroutine bitsplit_int32 ( i, n, d )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer( int32 )                          , intent ( in  )  :: i
    integer( int8  )                          , intent ( in  )  :: n
    real( double )  , dimension( 32_int8 / n ), intent ( out )  :: d
!
!.....Local variables
!
    integer( int8  )  :: j, bn
    integer( int32 )  :: s, sr
    real( double )    :: bt
!
! ----
!
!.....Check
!
    if ( n .gt. 32_int16 ) stop 'ERROR: bitsplit_int32 devider is larger than 32!'
    if ( n .lt.  0_int16 ) stop 'ERROR: bitsplit_int32 devider is smaller than 0!'
    if ( n .eq.  0_int16 ) stop 'ERROR: bitsplit_int32 devider is equal to 0!'
!
!.....Convert
!
    bn = 32_int8 / n
!
    do j = 1_int8, bn
      s  = 32_int32 - int( ( j - 1 ) * n, kind = int32 )
      sr = int( j * n, kind = int32 )
      if (  btest( i , s - 1_int32 )  ) then
        bt = 2._double ** n
      else
        bt = 0._double
      end if
      d( j ) = real( iand(  ishft( i, sr - 32_int32 ), 2 ** n - 1 ), kind = double ) - bt
    end do
!
    return
!
  End subroutine bitsplit_int32 
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine bitsplit
!
! This functions splits the B-bit number X into signed N-bit array.
!
!    - X must be unsigned integer class
!    - N ranges from 1 to B
!    - B is a multiple of N
!
!****************************
  Subroutine bitsplit ( i, b, n, d )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer( int32 )                    , intent ( in  )  :: i
    integer( int8  )                    , intent ( in  )  :: b, n
    real( double )  , dimension( b / n ), intent ( out )  :: d
!
!.....Local variables
!
    integer( int8  )  :: j, bn
    integer( int32 )  :: s, sr
    real( double )    :: bt
!
! ----
!
!.....Check
!
    if ( n .gt.       b  ) stop 'ERROR: bitsplit devider is larger than bit length!'
    if ( n .lt.  0_int16 ) stop 'ERROR: bitsplit devider is smaller than 0!'
    if ( n .eq.  0_int16 ) stop 'ERROR: bitsplit devider is equal to 0!'
!
!.....Convert
!
    bn = b / n
!
    do j = 1_int8, bn
      s  = int( b - ( j - 1 ) * n, kind = int32 )
      sr = int( j * n, kind = int32 )
      if (  btest( i , s - 1_int32 )  ) then
        bt = 2._double ** n
      else
        bt = 0._double
      end if
      d( j ) = real( iand(  ishft( i, sr - b ), 2 ** n - 1 ), kind = double ) - bt
    end do
!
    return
!
  End subroutine bitsplit 
!
!*****************************************************************************80

End module io_types
