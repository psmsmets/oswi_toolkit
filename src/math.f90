!*****************************************************************************80
!
!                                    M A T H
!
!  MODULE:       math.f90
!
!  Programmer:   Pieter S. M. Smets
!                Seismology Devision - Koninklijk Nederlands Meteorologisch Instituut (KNMI)
!                De Bilt, The Netherlands
!
!  Date:         August 29, 2014
!
!  Language:     Fortran-90
!
!  Description:  . 
!
!*****************************************************************************80

Module math

   use syms
   use io_types

!
!..define dms types
!
   type type_dms
      integer(int16) :: deg
      integer(int8)  :: min
      real(single)   :: sec
   end type type_dms
!

Contains

! =========================================================================================
!
!..Subroutine sph2llh
!
!  convert ph (azimuth), th (colatitude), and r (radius) to lon (longitude), lat
!  (latitude), and h (altitude) above the ellipsoid WGS84
!
!****************************
   Subroutine sph2llh ( ph, th, r, lat, lon, h, lonshift )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in  )  :: ph, th, r  
      real(double), intent( out )  :: lat, lon, h
      real(double), intent(in), optional  :: lonshift
!
! ----
!          
!.....Spherical convertion: radial distance r, polar angle θ (theta), and azimuthal angle φ (phi)
!
      if (present(lonshift)) then
        lon = lonrangef(ph * r2d) - lonshift ! longitude (deg)
      else
        lon = lonrangef(ph * r2d) ! longitude (deg)
      end if
      lat = 90._double - th * r2d    ! latitude (deg)
      h   = r - RE              ! altitude (km)
!
      return
!
   End subroutine sph2llh
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine llh2sph
!
!  convert lon (longitude), lat (latitude), and h (altitude) to ph (azimuth), 
!  th (colatitude), and r (radius) to 
!
!****************************
   Subroutine llh2sph ( lat, lon, h, ph, th, r, lonshift )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in  )  :: lat, lon, h
      real(double), intent( out )  :: ph, th, r
      real(double), intent(in), optional  :: lonshift
!
! ----
!          
!.....Spherical convertion: radial distance r, polar angle θ (theta), and azimuthal angle φ (phi)
!
      if (present(lonshift)) then
        ph = lonrangef(lon+lonshift) * d2r   ! longitude (deg)
      else
        ph = lonrangef(lon) * d2r   ! longitude (deg)
      end if
      th = ( 90._double - lat ) * d2r  ! latitude (deg)
      r   = re + h                ! altitude (km)
!
      return
!
   End subroutine llh2sph 
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine distance
!
!  get distance between to coordinates
!
!****************************
   Subroutine distance ( lat1, lon1, lat2, lon2, d, earthRadius )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )     :: lat1, lon1, lat2, lon2
      real(double), intent( out )    :: d
      integer, intent(in), optional      :: earthRadius
!
!.....local variables
!
      real(double)                   :: latA, lonA, latB, lonB
!
! ----
!        
!.....Degrees to radians transform
!
      latA = lat1 * d2r
      lonA = lon1 * d2r
      latB = lat2 * d2r
      lonB = lon2 * d2r
!
!.....Great circle distance, less subjective to rounding errors for small distances
!
      d = 2 * dasin(   dsqrt(  ( dsin(  ( latA - latB ) / 2  ) )**2 + & 
         & dcos( latA ) * dcos( latB ) * ( dsin(  ( lonA - lonB ) / 2  ) )**2  )   )
!
!.....Great circle distance to km
!
      if (present(earthRadius)) then
          d = d * earthRadius
      else
          d = d * RE
      end if
!
      return
!
   End subroutine distance
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine orthographic_projection
!
!  get orthographic projection of lat/lon coordinates
!
!****************************
   Subroutine orthographic_projection ( lat, lon, lat0, lon0, x, y, earthRadius )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent(in)      :: lat, lon, lat0, lon0
      real(double), intent(out)     :: x, y
      integer, intent(in), optional :: earthRadius
!
!.....local variables
!
      real(double)  :: R, phi, lambda, phi0, lambda0
!
! Init
!
  R=Re
  if (present(earthRadius)) R=earthRadius
!        
! Degrees to radians transform
!
  phi0    = lat0*d2r 
  lambda0 = lon0*d2r
  phi     = lat*d2r 
  lambda  = lon*d2r
!
! Orthographic projection: https://en.wikipedia.org/wiki/Orthographic_projection_in_cartography
!
  x = R * cos(phi)*sin(lambda-lambda0)
  y = R * ( cos(phi0)*sin(phi) - sin(phi0)*cos(phi)*cos(lambda-lambda0) )
!
   return
!
End subroutine orthographic_projection
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine orthographic_projection
!
!  get orthographic projection of lat/lon coordinates
!
!****************************
   Subroutine orthographic_projection_vec ( lat, lon, lat0, lon0, x, y, earthRadius )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent(in) , dimension(:)          :: lat, lon
      real(double), intent(in)                         :: lat0, lon0
      real(double), intent(out), dimension(size(lat))  :: x, y
      integer, intent(in), optional :: earthRadius
!
!.....local variables
!
      real(double)                        :: R, phi0, lambda0
      real(double), dimension(size(lat))  :: phi, lambda
!
! Check
!
  if (size(lat).ne.size(lon)) stop '@ orthographic_projection_vec : lat and lon vectors are not similar!'
!
! Init
!
  R=Re
  if (present(earthRadius)) R=earthRadius
!        
! Degrees to radians transform
!
  phi0    = lat0*d2r 
  lambda0 = lon0*d2r
  phi     = lat*d2r 
  lambda  = lon*d2r
!
! Orthographic projection: https://en.wikipedia.org/wiki/Orthographic_projection_in_cartography
!
  x = R * cos(phi)*sin(lambda-lambda0)
  y = R * ( cos(phi0)*sin(phi) - sin(phi0)*cos(phi)*cos(lambda-lambda0) )
!
   return
!
End subroutine orthographic_projection_vec
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine euclidean_distance 
!
!  get euclidean distance between two cartesian coordinates
!
!****************************
   Subroutine euclidean_distance ( x1, y1, z1, x2, y2, z2, d )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )   :: x1, y1, z1, x2, y2, z2 
      real(double), intent( out )  :: d
!
! ----
!
      d = dsqrt(  ( x1 - x2 ) ** 2 + ( y1 - y2 ) ** 2 + ( z1 - z2 ) ** 2  )
!
      return
!
   End subroutine euclidean_distance 
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine euclidean_distance_llh 
!
!  get euclidean distance between two geodetic coordinates
!
!****************************
   Subroutine euclidean_distance_llh ( lat1, lon1, h1, lat2, lon2, h2, d )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )   :: lat1, lon1, h1, lat2, lon2, h2 
      real(double), intent( out )  :: d
!          
!.....local variables
!
      real(double)   :: x1, y1, z1, x2, y2, z2
!
! ----
!
      call llh2cart( lat1, lon1, h1, x1, y1, z1 )
      call llh2cart( lat2, lon2, h2, x2, y2, z2 )
!
      call euclidean_distance( x1, y1, z1, x2, y2, z2, d )
!
      return
!
   End subroutine euclidean_distance_llh 
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine euclidean_distance_sph 
!
!  get euclidean distance between two spherical coordinates
!
!****************************
   Subroutine euclidean_distance_sph ( ph1, th1, r1, ph2, th2, r2, d )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )   :: ph1, th1, r1, ph2, th2, r2 
      real(double), intent( out )  :: d
!          
!.....local variables
!
      real(double)   :: x1, y1, z1, x2, y2, z2
!
! ----
!
!.....Spherical convertion: radial distance r, polar angle θ (theta), and azimuthal angle φ (phi)
!
      call sph2cart( ph1, th1, r1, x1, y1, z1 )
      call sph2cart( ph2, th2, r2, x2, y2, z2 )
!
      call euclidean_distance( x1, y1, z1, x2, y2, z2, d )
!
      return
!
   End subroutine euclidean_distance_sph 
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine sph2cart
!
!  Convert shperical to cartesian coordinates
!
!****************************
   Subroutine sph2cart ( ph, th, r, x, y, z )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )   :: ph, th, r
      real(double), intent( out )  :: x, y, z
!
! ----
!
!.....Spherical convertion: radial distance r, polar angle θ (theta), and azimuthal angle φ (phi)
!
      x = r * dsin( th ) * dcos( ph )
      y = r * dsin( th ) * dsin( ph )
      z = r * dcos( th )
!
      return
!
   End subroutine sph2cart 
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine cart2sph
!
!  Convert cartesian to spherical coordinates 
!
!****************************
   Subroutine cart2sph ( x, y, z, ph, th, r )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )   :: x, y, z
      real(double), intent( out )  :: ph, th, r
!
! ----
!
!.....Spherical convertion: radial distance r, polar angle θ (theta), and azimuthal angle φ (phi)
!
      r  = dsqrt( x ** 2 + y ** 2 + z ** 2 )
      th = dacos( z / r )
      ph = datan2( y, x )
!
      return
!
   End subroutine cart2sph 
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine llh2cart 
!
!  Convert lat lon to cartesian coordinates
!
!****************************
   Subroutine llh2cart ( lat, lon, h, x, y, z )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )   :: lat, lon, h
      real(double), intent( out )  :: x, y, z
!          
!.....local variables
!
      real(double)  :: ph, th, r
!
! ----
!
      call llh2sph( lat, lon, h, ph, th, r )
      call sph2cart( ph, th, r, x, y, z )
!
      return
!
   End subroutine llh2cart 
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine cart2llh
!
!  Convert cartesian to lat lon coordinates 
!
!****************************
   Subroutine cart2llh ( x, y, z, lat, lon, h )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )   :: x, y, z
      real(double), intent( out )  :: lat, lon, h
!          
!.....local variables
!
      real(double)  :: ph, th, r
!
! ----
!
      call cart2sph( x, y, z, ph, th, r )
      call sph2llh( ph, th, r, lat, lon, h )
!
      return
!
   End subroutine cart2llh 
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine angle
!
!  get angle between two coordinates
!
!****************************
   Subroutine angle ( lat1, lon1, lat2, lon2, a )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )     :: lat1, lon1, lat2, lon2
      real(double), intent( out )    :: a
!
!.....local variables
!
      real(double)                 :: latA, lonA, latB, lonB
!
! ----
!        
!.....Degrees to radians transform
!
      latA = lat1 * d2r
      lonA = lon1 * d2r
      latB = lat2 * d2r
      lonB = lon2 * d2r
!
!.....Bearing angle
!
      a = datan2(  &
         &   dsin( lonB - lonA ) * dcos( latB ) , &
         &   dcos( latA ) * dsin( latB ) - dsin( latA ) * dcos( latB ) * dcos( lonB - lonA ) &
         & )
!
!.....Course to degrees
!
      a = a * r2d
!
      return
!
   End subroutine angle
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine course
!
!  get the great circle distance and bearing angle between two coordinates
!
!****************************
   Subroutine course ( lat1, lon1, lat2, lon2, d, a )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )     :: lat1, lon1, lat2, lon2
      real(double), intent( out )    :: d, a
!
!.....local variables
!
      real(double)                 :: latA, lonA, latB, lonB
!
! ----
!        
!.....Degrees to radians transform
!
      latA = lat1 * d2r
      lonA = lon1 * d2r
      latB = lat2 * d2r
      lonB = lon2 * d2r
!
!.....Great circle distance, less subjective to rounding errors for small distances
!
      d = 2 * dasin( dsqrt(   &
         &   (  dsin( ( latA - latB ) / 2 )  )**2 + & 
         &   dcos( latA ) * dcos( latB ) * (  dsin( ( lonA - lonB ) / 2 )  )**2 &
         & ) )
!
!.....Bearing angle
!
      a = datan2(  &
         &   dsin( lonB - lonA ) * dcos( latB ) , &
         &   dcos( latA ) * dsin( latB ) - dsin( latA ) * dcos( latB ) * dcos( lonB - lonA )  &
         & )
!
!.....Great circle distance to km
!
      d = d * RE
!
!.....Course to degrees
!
      a = a * r2d
!
      return
!
   End subroutine course
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine radial_distance 
!
!  get the end coordinate given a start coordinate and a distance and bearing
!
!****************************
   Subroutine radial_distance ( lat1, lon1, d, a, lat2, lon2, a2 )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )            :: lat1, lon1, a, d
      real(double), intent( out )           :: lat2, lon2
      real(double), intent( out ), optional :: a2
!
!.....local variables
!
      real(double)                 :: az, di, lat, lon, az2
      real(double), parameter      :: EPS = 1.0_double / 10**6
!
! ----
!
!.....Degrees to radians transform
!
      lat = lat1 * d2r
      lon = lon1 * d2r
      az  = a * d2r
      di  = d * d2r
!
!.....A point (lat1,lon1) is a distance d out on the tc radial from point (lat0,lon0)
!
      lat2 = dasin(  dsin( lat ) * dcos( di ) + dcos( lat ) * dsin( di ) * dcos( az )  )
!
      if ( dcos( lat2 ) .LE. EPS ) then
         lon2 = lon ! endpoint is a pole
      else
         lon2 = lon + datan2(  &
            &   dsin( az ) * dsin( di ) * dcos( lat ), &
            &   dcos( di ) - dsin( lat ) * dsin( lat2 ) &
            & )
      end if
!
!.....Radians to degrees
!
      lat2 = lat2 * r2d
      lon2 = lon2 * r2d
!
!.....bearing angle of final point
!
      if ( present( a2 ) ) then
         CALL ANGLE ( lat2, lon2, lat1, lon1, az2 )
         a2 = lonrangeF ( az2 + 180._double )
      end if
!
      return
!
   End subroutine radial_distance
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine sphdistance
!
!  get distance between to coordinates
!
!****************************
   Subroutine sphdistance ( ph1, th1, ph2, th2, d )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )     :: ph1, th1, ph2, th2
      real(double), intent( out )    :: d
!
! ----
!
!.....Great circle distance, less subjective to rounding errors for small distances
!
      d = 2 * dasin(   dsqrt(  ( dsin(  ( th1 - th2 ) / 2  ) )**2 + & 
         & dsin( th1 ) * dsin( th2 ) * ( dsin(  ( ph1 - ph2 ) / 2  ) )**2  )   )
!
!.....Great circle distance to km
!
      d = d * RE
!
      return
!
   End subroutine sphdistance
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine twopirange
!
!  check and correct longitude to be within [0,twopi) rad interval.
!
!****************************
   Subroutine twopirange ( ph, offset )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( inout )        :: ph
      real(double), intent( in ), optional :: offset
!
!.....local variables
!
      real(double) :: o
!
! ----
!
!.....Set offset
!
      o = 0._double
      if ( present( offset ) ) o = offset
!
!.....Degrees to radians transform
!
      ph = ph - floor(  ( ph - o ) / twopi  ) * twopi
!
      return
!
   End subroutine twopirange
!
! =========================================================================================


! =========================================================================================
!
!..Function twopirangef
!
!  check and correct longitude to be within [0,twopi) rad interval.
!
!****************************
   Function twopirangef ( ph, offset ) RESULT ( ph2 )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )           :: ph
      real(double)                         :: ph2
      real(double), intent( in ), optional :: offset
!
!.....local variables
!
      real(double) :: o = 0._double
!
! ----
!
      ph2=ph
      call twopirange( ph2, offset )
!
      return
!
   End function twopirangef
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine npole_range
!
!  check and correct latitude at north pole.
!
!****************************
   Subroutine npole_range ( ph, th, lpole )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      logical, intent( out )            :: lpole
      real(double), intent( inout ) :: ph, th
!
!.....local variables
!
      real(double) :: ph2
!
! ----
!
!.....Correct th
!
      if ( th .LT. 0._double ) then
         th  = dabs( th )
         ph2 = ph + pi
         ph = twopirangef( ph2 )
         lpole = .true.
      else
         lpole = .false.
      end if
!
      return
!
   End subroutine npole_range
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine spole_range
!
!  check and correct latitude at south pole.
!
!****************************
   Subroutine spole_range ( ph, th, lpole )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      logical, intent( out )            :: lpole
      real(double), intent( inout ) :: ph, th
!
!.....local variables
!
      real(double) :: ph2
!
! ----
!
!.....Correct th
!
      if ( th .gt. pi ) then
         th = twopi - th
         ph2 = ph + pi
         ph = twopirangef( ph2 )
         lpole = .true.
      else
         lpole = .false.
      end if
!
      return
!
   End subroutine spole_range
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine lonrange
!
!  check and correct longitude to be within [0,360) degrees interval.
!
!****************************
   Subroutine lonrange ( lon, offset )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( inout )        :: lon
      real(double), intent( in ), optional :: offset
!
!.....local variables
!
      real(double) :: o
!
! ----
!
!.....Set offset
!
      if ( present(offset) ) then
        o = offset
      else
        o = 0._double
      end if
!
!.....Degrees to radians transform
!
      lon = lon - floor(  ( lon - o ) / 360._double  ) * 360._double
!
      return
!
   End subroutine lonrange
!
! =========================================================================================


! =========================================================================================
!
!..Function lonrangeF
!
!  check and correct longitude to be within [0,360) degrees interval.
!
!****************************
   Function lonrangeF ( lon, offset ) RESULT ( lon2 )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in )           :: lon
      real(double)                         :: lon2
      real(double), intent( in ), optional :: offset
!
!.....local variables
!
      real(double) :: o
!
! ----
!
      lon2=lon
      call lonrange( lon2, offset )
!
      return
!
   End function lonrangeF
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine latrange 
!
!  check and correct latitdue to be within [-90,90] degrees interval.
!
!****************************
   Subroutine latrange ( lat, lon )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( inout ) :: lat, lon
!
! ----
!
!.....Degrees to radians transform
!
      if (  dabs( lat ) .gt. 90._double ) then
         lat = sign( 1._double, lat ) * 180._double - lat
         lon = lonrangeF( lon + 180._double )
      end if
!
      return
!
   End subroutine latrange
!
! =========================================================================================



! =========================================================================================
!
!..Function degrees2dms
!
!  Convert decimal degrees to degrees-minutus-seconds
!
!****************************
   Subroutine degrees2dms ( deg, dms, round_sec )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double)  , intent(in)             :: deg
      type(type_dms), intent(out)            :: dms
      logical       , intent(in) , optional  :: round_sec
!
!.....Local variables
!
      logical       :: rnd
      real(single)  :: dummy
!
! ----
!
      rnd=.false.
      if (present(round_sec)) rnd=round_sec
!
      if (rnd) then
         dummy=real(anint(deg*3600._double,kind=double)/3600._double,kind=single)
      else
         dummy=real(deg,kind=single)
      end if
!
      dms%deg=int(dummy,kind=int16)
      dummy=(dummy-dms%deg)*60
      dms%min=int(dummy,kind=int8)
      dms%sec=real((dummy-dms%min)*60,kind=single)
!
      return
!
   End subroutine degrees2dms
!
! =========================================================================================


! =========================================================================================
!
!..Function deg2dms
!
!  Convert decimal degrees to degrees-minutus-seconds
!
!****************************
   Function deg2dms ( deg ) result( dms )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      type(type_dms)                        :: dms
      real(double)  , intent(in)            :: deg
!
! ----
!
      call degrees2dms( deg, dms, .false. )
!
      return
!
   End function deg2dms
!
! =========================================================================================


! =========================================================================================
!
!..Function deg2dmsr
!
!  Convert decimal degrees to degrees-minutus-seconds
!
!****************************
   Function deg2dmsr ( deg ) result( dms )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      type(type_dms)                        :: dms
      real(double)  , intent(in)            :: deg
!
! ----
!
      call degrees2dms( deg, dms, .true. )
!
      return
!
   End function deg2dmsr
!
! =========================================================================================


! =========================================================================================
!
!..Function dms2deg
!
!  Convert degrees-minutus-seconds to decimal degrees
!
!****************************
   Function dms2deg( dms ) result( deg )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double)                :: deg
      type(type_dms), intent(in)  :: dms
!
! ----
!
      deg = real(dms%deg,kind=double)+real(dms%min,double)/60._double+dms%sec/3600._double
!
      return
!
   End function dms2deg
!
! =========================================================================================


! =========================================================================================
!
!..Function dms2str
!
!   Print degrees-minutus-seconds to string
!
!****************************
   Function dms2str( dms ) result( str )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      character(len=20)           :: str
      type(type_dms), intent(in)  :: dms
!
!.....Local variables
!
      character  :: d*4, m*2, s*6
!
! ----
!
      write(d,"(i4)") dms%deg
      write(m,"(i2)") dms%min
      write(s,"(f6.3)") dms%sec
!
      str=d//'d '//m//'m '//s//'s'
!
      return
!
   End function dms2str
!
! =========================================================================================

! =========================================================================================
!
!..Function sec 
!
!  Secans
!
!****************************
   real(double) Function sec ( a )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in ) :: a
!
! ----
!
      sec = 1._double / dsin( a )
!
      return
!
   End function sec
!
! =========================================================================================


! =========================================================================================
!
!..Function csc 
!
!  Cosecans
!
!****************************
   real(double) Function csc ( a )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in ) :: a
!
! ----
!
      csc = 1._double / dcos( a )
!
      return
!
   End function csc
!
! =========================================================================================


! =========================================================================================
!
!..Function cot 
!
!  Cotangens
!
!****************************
   real(double) Function cot ( a )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), intent( in ) :: a
!
! ----
!
      cot = 1._double / dtan( a )
!
      return
!
   End function cot
!
! =========================================================================================


! =========================================================================================
!
!..Subroutine smooth 
!
!****************************
   Subroutine smooth ( y, span )
!****************************
!
      implicit none
!          
!.....dummy variables
!
      real(double), dimension(:), intent( inout )            :: y
      integer(int32)                         , intent( in    ), optional  :: span
!
!.....local variables
!
      integer(int32) :: nofy, i, s, nofb
      real(double), dimension ( size( y, 1 ) ) :: yy
!
! ----
!
!.....set span
!
      if ( present( span ) ) then
         if ( mod( span, 2 ) .EQ. 0 ) then
            s = span + 1
         else
            s = 5
         end if
      else
         s = 5
      end if
!
!.....get length
!
      nofy = size( y, 1 )
!
      if ( nofy .LE. span .or. span .LT. 3 ) return
!
!.....boundary points
!
      nofb = ( s - 1 ) / 2
!
      do i = 0, nofb - 1
        yy(    1 + i ) = sum(  y( 1 : 2 * i + 1 )  ) / ( 2 * i + 1 ) 
        yy( nofy - i ) = sum(  y( nofy - 2 * i : nofy )  ) / ( 2 * i + 1 )
      end do
!
!.....regular values
!
      do i = nofb + 1, nofy - nofb
         yy( i ) = sum( y ( i - 2 : i + 2 ) ) / 5
      end do
!
!.....copy
!
      y = yy
!
      return
!
   End subroutine smooth 
!
! =========================================================================================


Subroutine CTBIP( x, y, z, xc, yc, zc, alpha, beta, m, mc )
!*****************************************************************************80
!
!..Subroutine CTBIP
!
!  get convex tetragon bilinear interpolation parameters
!
!****************************
!
  implicit none
!          
! dummy variables
!
  real(double), intent(in) , dimension(4)  :: x, y, z
  real(double), intent(in)                 :: xc, yc
  real(double), intent(out)                :: zc
  real(double), intent(out), optional      :: alpha, beta
  logical, intent(in) , optional, dimension(4) :: m
  logical, intent(out), optional               :: mc
!
! local variables
!
  real(double)  :: u, v


  call CTBIP_COEF( x, y, xc, yc, u, v, m, mc )
  if (present(mc)) then
    if(.not.mc) return
  end if
  call EVAL_CTBIP_COEF( z, u, v, zc )
!
  if (present(alpha)) alpha=u
  if (present(beta)) beta=v
!
   return
!
!*****************************************************************************80
End subroutine CTBIP


Subroutine CTBIP_COEF( x, y, xc, yc, alpha, beta, m, mc )
!*****************************************************************************80
!
!..Subroutine CTBIP
!
!  get convex tetragon bilinear interpolation parameters
!
!****************************
!
  implicit none
!          
! dummy variables
!
  real(double), intent(in) , dimension(4)  :: x, y
  real(double), intent(in)                 :: xc, yc
  real(double), intent(out), target        :: alpha, beta
  logical, intent(in) , optional, dimension(4) :: m
  logical, intent(out), optional               :: mc
!
! local variables
!
  real(double)           :: ax, bx, cx, dx, ay, by, cy, dy, k0, k1, k2, k3, D
  real(double), pointer  :: u, v
! http://stackoverflow.com/questions/808441/inverse-bilinear-interpolation
! http://stackoverflow.com/questions/23920976/bilinear-interpolation-with-non-aligned-input-points
! http://www.iquilezles.org/www/articles/ibilinear/ibilinear.htm

!
  u=>alpha
  v=>beta
!
!  zc=-9999.d0
  u=-1.d0
  v=-1.d0
!
  ax = x(2) - x(1)
  bx = x(3) - x(1)
  cx = x(1) - x(2) - x(3) + x(4)
  dx = xc - x(1)
!
  ay = y(2) - y(1)
  by = y(3) - y(1)
  cy = y(1) - y(2) - y(3) + y(4)
  dy = yc - y(1)
!
  k3 = ax + cx*u
  k2 = cx*by - cy*bx
  k1 = ax*by - ay*bx + dx*cy - dy*cx
  k0 = dx*ay - dy*ax
!
  D = k1**2 - 4*k0*k2
  if (D.lt.0.d0.or.abs(k2).lt.1.d-8.or.abs(k3).lt.1.d-8) return
  D = sqrt(D)
!
! solution 1
!
  u = (-k1 - D)/(2*k2)
  v  = (dx - bx*u)/k3 
!
! solution 2?
!
  if( u.lt.0.d0 .or. u.gt.1.d0 .or. v.lt.0.d0 .or. v.gt.1.d0 ) then
    u = (-k1 + D)/(2*k2)
    v  = (dx - bx*u)/k3
    if( u.lt.0.d0 .or. u.gt.1.d0 .or. v.lt.0.d0 .or. v.gt.1.d0 ) then
      u=-1.d0
      v=-1.d0
      return
    end if
  end if
!
! mask okay?
!
  if (present(m)) then
    if (present(mc)) mc=.false.
    if ((u.lt.1.d0.and.v.lt.1.d0).and..not.m(1)) return
    if ((u.gt.0.d0.and.v.lt.1.d0).and..not.m(2)) return
    if ((u.lt.1.d0.and.v.gt.0.d0).and..not.m(3)) return
    if ((u.gt.0.d0.and.v.gt.0.d0).and..not.m(4)) return
    if (present(mc)) mc=.true.
  end if
!
  return
!
!*****************************************************************************80
End subroutine CTBIP_COEF


Subroutine EVAL_CTBIP_COEF( z, alpha, beta, zc, m, mc )
!*****************************************************************************80
!
  implicit none
!          
! dummy variables
!
  real(double), intent(in) , dimension(4)  :: z
  real(double), intent(in) , target        :: alpha, beta
  real(double), intent(out)                :: zc
  logical, intent(in) , optional, dimension(4) :: m
  logical, intent(out), optional               :: mc
!
! local variables
!
  real(double), pointer  :: u, v
!
  if (present(m)) then
    u=>alpha
    v=>beta
    if (present(mc)) mc=.false.
    if ((u.lt.1.d0.and.v.lt.1.d0).and..not.m(1)) return
    if ((u.gt.0.d0.and.v.lt.1.d0).and..not.m(2)) return
    if ((u.lt.1.d0.and.v.gt.0.d0).and..not.m(3)) return
    if ((u.gt.0.d0.and.v.gt.0.d0).and..not.m(4)) return
    if (present(mc)) mc=.true.
  end if
!
  zc = (1 - alpha) * ((1 - beta) * z(1) + beta * z(3) ) &
       + alpha * ((1 - beta) * z(2) + beta * z(4) )
!
   return
!
!*****************************************************************************80
End subroutine EVAL_CTBIP_COEF

End module math
