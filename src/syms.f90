! =========================================================================================
!
! MODULE SYMS
!
! --> All frequently used symbols.
!
! =========================================================================================
!
Module syms
!
!.....Frequently used mathematical constants 
!
      double precision, parameter :: pi    = 3.141592653589793238462643383279502884197d0
      double precision, parameter :: pio2  = 1.57079632679489661923132169163975144209858d0
      double precision, parameter :: twopi = 6.283185307179586476925286766559005768394d0
      double precision, parameter :: sqrt2 = 1.41421356237309504880168872420969807856967d0
      double precision, parameter :: euler = 0.5772156649015328606065120900824024310422d0
!
!.....Some scalars
!
      double precision, parameter :: a6th = 0.166666666666666666666666666666666d0
!
!.....Frequently used geological constants
!
      double precision, parameter :: re   = 6367.470d0 ! radius of earth (km)
      double precision, parameter :: rea  = 6378.160d0
      double precision, parameter :: reb  = 6356.775d0
      double precision, parameter :: reab = rea * reb
      double precision, parameter :: ref  = 1.d0 / 297.d0
      double precision, parameter :: ree  = 0.0820944379497d0
      double precision, parameter :: d2r  = pi / 180.d0
      double precision, parameter :: r2d  = 180.d0 / pi
      double precision, parameter :: g    = 9.80665d0
!
! =========================================================================================
!
End module syms 
