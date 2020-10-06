!*****************************************************************************80
!
!                                       T I M E
!
!  Module:       TIME
!
!  Programmer:   Pieter S. M. Smets
!                Seismology Devision - Koninklijk Nederlands Meteorologisch
!                Instituut (KNMI), De Bilt, The Netherlands
!
!  Date:         June 20, 2017
!
!  Language:     Fortran-90
!
!  Description:  
!
!*****************************************************************************80

Module time

  use io_types

!
!..Shared module variables
!
   character(len=*), parameter  :: tstr_date_units = "Gregorian date"
   character(len=*), parameter  :: tstr_time_units = "Coordinated Universal Time"
   character(len=*), parameter  :: tstr_units      = tstr_date_units // " and " // tstr_time_units
   character(len=*), parameter  :: time_epoch      = "1970/01/01 00:00:00.000000"
   character(len=*), parameter  :: time_units      = "Microseconds sinds " // time_epoch // " UTC"
   integer(int32)  , parameter  :: time_scalar     = 1000000_int32
   integer(int32)  , parameter  :: time_len        = 26_int32
   character(len=*), parameter  :: matlab_time_units = "Days sinds 0000/00/00 00:00:00.000000 UTC"
!
!..Private module variables
!
   integer(int16), dimension(12), private, parameter  :: cmonthsize = &
                  (/   0_int16,  31_int16,  59_int16,  90_int16, 120_int16, 151_int16, &
                     181_int16, 212_int16, 243_int16, 273_int16, 304_int16, 334_int16  /)
!
   integer(int8), dimension(12), private, parameter  :: monthsize = &
                  (/   31_int8,  28_int8,  31_int8,  30_int8, 31_int8, 30_int8, &
                     31_int8, 31_int8, 30_int8, 31_int8, 30_int8, 31_int8  /)
!
   integer(int8), dimension(12), private, parameter  :: monthsize_ly = &
                  (/   31_int8,  29_int8,  31_int8,  30_int8, 31_int8, 30_int8, &
                     31_int8, 31_int8, 30_int8, 31_int8, 30_int8, 31_int8  /)
!
   integer(int32), private, parameter  :: base_year   = 1970_int32
   integer(int32), private, parameter  :: base_month  = 1_int32
   integer(int32), private, parameter  :: base_day    = 1_int32
   integer(int32), private, parameter  :: base_hour   = 0_int32
   integer(int32), private, parameter  :: base_min    = 0_int32
   real(double)  , private, parameter  :: base_sec    = 0._double
   integer(int32), private, parameter  :: base_julday = 719529_int32
   integer(int64), private, parameter  :: base_s2unit = int(time_scalar,int64) ! microseconds
   integer(int64), private, parameter  :: base_epoch  = &
      int( &
        ( (cmonthsize(base_month)+base_day-1_int32)*86400_int32 &
        + base_hour*3600_int32 + base_min*60_int32 ) * base_s2unit &
        + int(base_sec*base_s2unit,int64) ,int64) ! add a day for leapyear !

Contains

Function timestamp ( ) result(tstr)
!*****************************************************************************80
!
!! TIMESTAMP
!
!  Description:
!
!    Write current date and time to string according to ISO 8601
!    Example: 2017-01-26T03:26:06+01:00
!
!  Modified:
!
!    26 January 2017
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    ...
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  character(len=25) :: tstr
!
! Local variables
!
  integer(int32), dimension(8) :: dtvalues
  character(len=6)  :: utc_offset
!
! Get date and time
!
  call date_and_time(values=dtvalues)
!
! Format utc offset
!
  write(utc_offset,"(i2.2,':',i2.2)") nint(dtvalues(4)/60.d0), dtvalues(4)-nint(dtvalues(4)/60.d0)*60
  if (dtvalues(4).lt.0_int32) utc_offset='-'//trim(utc_offset)
  if (dtvalues(4).ge.0_int32) utc_offset='+'//trim(utc_offset)
!
! Format all
!
  write(tstr,"(i4.4,2('-',i2.2),'T',i2.2,2(':',i2.2),a6)") dtvalues(1:3),dtvalues(5:7),utc_offset
!
  return
!
!*****************************************************************************80
End function timestamp


! =========================================================================================
!
   Subroutine time2epoch ( year, month, day, h, m, s, epoch )
!
      implicit none
!
!  dummy variables
!
      integer(int32), intent(in ) :: year, month, day, h, m
      real(double)  , intent(in ) :: s
      integer(int64), intent(out) :: epoch
!
!  local variables
!
      integer(int32)  :: daynum
!
! ---
!
      call daynumber( year, month, day, daynum )
!
      epoch = int( daynum*86400 + h*3600 + m*60, int64 ) * base_s2unit
      epoch = epoch + nint( s*base_s2unit, int64 ) - base_epoch
!
      return
!
   End subroutine time2epoch
!
! =========================================================================================


! =========================================================================================
!
   Subroutine doy2monthday ( doy, month, day, leapyr, year )
!
      implicit none
!
!.....dummy variables
!
      integer(int32), intent(in )            :: doy
      logical       , intent(in ), optional  :: leapyr
      integer(int32), intent(in ), optional  :: year
      integer(int32), intent(out)            :: month, day
!
!.....local variables
!
      logical  :: ly
!
! ---
!
!.....Leapyear? February 29 days!
!
      ly=.false.
      if ( present( leapyr ) ) ly = leapyr
      if ( present( year   ) ) ly = leapyear(year)
!
!.....copy day of year
!
      day   = doy
      month = 1_int32
!
!.....check
!
      if ( doy.lt.1_int32   ) stop 'ERROR @ doy2monthday : doy < 1'
      if ( doy.gt.365_int32.and..not.ly ) stop 'ERROR @ doy2monthday : doy > 365 and no leapyear'
      if ( doy.gt.366_int32.and.ly ) stop 'ERROR @ doy2monthday : doy > 366 and leapyear'
!
!.....loop
!
       if (ly) then
          do while ( day .gt. monthsize_ly ( month ) )
             day   = day - monthsize_ly( month )
             month = month + 1_int32
          end do
       else
          do while ( day .gt. monthsize ( month ) )
             day   = day - monthsize( month )
             month = month + 1_int32
          end do
       end if
!
       return
!
   End subroutine doy2monthday
!
! =========================================================================================


! =========================================================================================
!
   Subroutine monthday2doy ( month, day, doy, leapy, year )
!
      implicit none
!
!.....dummy variables
!
      logical       , intent(in ), optional  :: leapy
      integer(int32), intent(in ), optional  :: year
      integer(int32), intent(in )            :: month, day
      integer(int32), intent(out)            :: doy
!
! ---
!
!.....copy day of year
!
      doy = cmonthsize(month) + day
!
!.....Leapyear? February 29 days!
!
      if ( present(leapy) ) then
         if ( leapy .and. month.gt.2_int32 ) doy = doy + 1_int32
      elseif (present(year)) then
         if (leapyear(year).and.month.gt.2_int32) doy = doy + 1_int32
      end if
!
   End subroutine monthday2doy
!
! =========================================================================================


! =========================================================================================
!
   Function yearsize ( leapy )
!
      implicit none
!
!  dummy variables
!
      logical, intent(in)  :: leapy 
      integer(int32)       :: yearsize
!
! ---
!
      if ( leapy ) then
         yearsize = 366_int32
      else
         yearsize = 365_int32
      end if
!
      return
!
   End function yearsize
!
! =========================================================================================


! =========================================================================================
!
   Function leapyear ( year )
!
      implicit none
!
!  dummy variables
!
      logical                     :: leapyear
      integer(int32), intent(in)  :: year
!
! ---
!
      if ( mod( year, 400_int32 ) .eq. 0_int32 ) then
         leapyear = .true.
      else if ( mod( year, 100_int32 ) .eq. 0_int32 ) then
         leapyear = .false.
      else if ( mod( year, 4_int32 ) .eq. 0_int32 ) then
         leapyear = .true.
      else
         leapyear = .false.
      end if
!
      return
!
   End function leapyear
!
! =========================================================================================


! =========================================================================================
!
   Subroutine daynumber ( year, month, day, daynum )
!
      implicit none
!
!.....dummy variables
!
      integer(int32), intent(in )  :: year, month, day
      integer(int32), intent(out)  :: daynum
!
!.....local variables
!
      integer(int32)  :: doy, y
!
! ---
!
!.....initialize
!
      daynum = 0_int32
!
      if ( year.gt.base_year ) then
         do y = base_year, year - 1_int32
            daynum = daynum + yearsize ( leapyear(y) )
         end do
      end if
!
      call monthday2doy( month, day, doy, leapyear(year) )
      daynum = daynum + doy - 1_int32
!
      return
!
   End subroutine daynumber
!
! =========================================================================================


! =========================================================================================
!
   Function julianday( year, month, day ) result( daynum )
!
      implicit none
!
!.....dummy variables
!
      integer(int32), intent(in )  :: year, month, day
      integer(int32)               :: daynum
!
!.....local variables
!
      integer(int32)  :: doy, y
!
! ---
!
!.....initialize
!
      daynum = 0_int32
!
      do y = 0_int32, year - 1_int32
         daynum = daynum + yearsize( leapyear(y) )
      end do
!
      call monthday2doy( month, day, doy, leapyear(year) )
      daynum = daynum + doy - 1_int32
!
      return
!
   End function julianday
!
! =========================================================================================


! =========================================================================================
!
   Function weekday ( year, month, day )
!
      implicit none
!
!.....dummy variables
!
      integer(int32), intent( in )            :: year
      integer(int32), intent( in ), optional  :: month, day
!
!.....local variables
!
      integer(int32)                          :: weekday, daynum
!
! ---
!
      if ( present(month) .and. present(day) ) then
         call daynumber ( year, month, day, daynum ) 
      else if ( .not.present(month) .and. .not.present(day) ) then
      else
         stop ' @ WEEKDAY : daynumber or year, month, day is required!'
      end if
!
      weekday = mod( daynum + 3_int32, 7_int32 ) + 1_int32
!
      return
!
   End function weekday
!
! =========================================================================================


! =========================================================================================
!
   Subroutine epoch2time ( epoch, year, month, day, h, m, s )
!
      implicit none
!
!  dummy variables
!
      integer(int64), intent(in)             :: epoch
      integer(int32), intent(out)            :: year, month, day
      integer(int32), intent(out), optional  :: h, m
      real(double)  , intent(out), optional  :: s
!
!  local variables
!
      logical         :: leap_year
      integer(int32)  :: year_size, daynum
      integer(int64)  :: epoch_, dayclock
!
! ---
!
!.....split epoch in time and daynumber
!
      epoch_ = epoch - base_epoch
!
      daynum   = int( epoch_ / 86400 / base_s2unit, int32 ) + 1_int32
      dayclock = mod( epoch_ , 86400*base_s2unit )
!
!.....hour, min, sec
!
      if ( present ( s ) ) s = real( mod( dayclock, 60*base_s2unit ), double ) / base_s2unit
      if ( present ( m ) ) m = int( mod( dayclock, 3600*base_s2unit ) / 60 / base_s2unit, int32 )
      if ( present ( h ) ) h = int( dayclock / 3600 / base_s2unit, int32 )
!
!.....get year
!
      year = base_year
!
!.....correct for base year offset
!
      daynum = daynum
!
      do
         leap_year = leapyear( year )
         year_size = yearsize( leap_year )
         if ( daynum .le. year_size ) exit
         daynum = daynum - year_size
         year   = year + 1_int32
      end do
!
!.....Get month and day
!
      call doy2monthday ( daynum, month, day, leap_year )
!
      return
!
   End subroutine epoch2time
!
! =========================================================================================


! =========================================================================================
!
   Subroutine time2str ( year, month, day, h, m, s, str )
!
      implicit none
!
!  dummy variables
!
      integer(int32)   , intent(in )  :: year, month, day, h, m
      real(double)     , intent(in )  :: s
      character(len=23), intent(out)  :: str
!
!.....local variables
!
      integer(int32)     :: s_
      real(double)       :: r_
!
! ---
!
      s_ = int( s, int32 )
      r_ = s-s_
!
      write (str,"(i4.4,a1,4(i2.2,a1),i2.2,f4.3)") year,"-",month,"-",day," ",h,":",m,":",s_,r_
!
      return
!
   End subroutine time2str
!
! =========================================================================================


! =========================================================================================
!
   Subroutine epoch2str ( epoch, str, space, date, time, nospace )
!
      implicit none
!
!  dummy variables
!
      integer(int64)   , intent(in)             :: epoch
      character(len=*) , intent(out)            :: str
      character(len=* ), intent(in ), optional  :: space, date, time
      logical          , intent(in ), optional  :: nospace
!
!  local variables
!
      logical            :: trims
      character          :: divs*1, divd*1, divt*1, str_fmt*99
      integer(int32)     :: year, month, day, h, m, s_
      real(double)       :: s, r_
      character(len=time_len)  :: str_
!
! ---
!
      call epoch2time ( epoch, year, month, day, h, m, s )
!
      s_ = int( s, int32 )
      r_ = s-s_
!
!.....default space characters
!
      divs  = ' '
      divd  = '-'
      divt  = ':'
      trims = .false.
!
!.....update?
!
      if ( present( space    ) ) divs = space
      if ( present( date     ) ) divd = date
      if ( present( time     ) ) divt = time
      if ( present( nospace  ) ) trims = nospace
!
      str_ = ''
      if (trims) then
         str_fmt = "(i4.4,'"//trim(divd)//"',i2.2,'"//trim(divd)//"',i2.2,'"//trim(divs)&
            &//"',2(i2.2,'"//trim(divt)//"'),i2.2,f7.6)"
      else
         str_fmt = "(i4.4,'"//trim(divd)//"',i2.2,'"//trim(divd)//"',i2.2,'"//divs&
            &//"',2(i2.2,'"//trim(divt)//"'),i2.2,f7.6)"
      end if
      write (str_,trim(str_fmt)) year, month, day, h, m, s_, r_
!
!.....trim
!
      if (len(str).lt.len_trim(str_)) then
         str=str_(1:len(str))
      else
         str(1:26)=str_
      end if
!
      return
!
   End subroutine epoch2str
!
! =========================================================================================


! =========================================================================================
!
   Subroutine str2time ( str, year, month, day, h, m, s )
!
      implicit none
!
!  dummy variables
!
      character(len=*), intent(in )            :: str
      integer(int32)  , intent(out), optional  :: year, month, day, h, m
      real(double)    , intent(out), optional  :: s
!
!  local variables
!
      integer(int32)  :: year_, month_, day_, h_, m_, ix, ix0, ix1, nofchars, i
      real(double)    :: s_
!
! ---
!
!.....number of characters in str
!
      nofchars = len_trim( str )
!
!.....initialize
!
      year_  = 0_int32
      month_ = 0_int32
      day_   = 0_int32
!
      h_     = 0_int32
      m_     = 0_int32
      s_     = 0._double
!
      ix     = 0_int32
      ix0    = 1_int32
!
!.....loop over characters
!
      do i = 1_int32, nofchars
         if ( iachar( str(i:i) ).lt.48_int32 .or. iachar( str(i:i) ).gt.57_int32 .or. i.eq.nofchars ) then
            ix = ix + 1_int32
            if ( i .eq. nofchars ) then
               ix1 = i
            else
               ix1 = i - 1_int32
            endif
!
!...........year, month, day
!
            if ( ix.eq.1_int32 ) read( str(ix0:ix1), * ) year_
            if ( ix.eq.2_int32 ) read( str(ix0:ix1), * ) month_
            if ( ix.eq.3_int32 ) read( str(ix0:ix1), * ) day_
!
!...........hour, min, sec
!
            if ( ix.eq.4_int32 ) read( str(ix0:ix1), * ) h_
            if ( ix.eq.5_int32 ) read( str(ix0:ix1), * ) m_
            if ( ix.eq.6_int32 ) then
               read( str(ix0:nofchars), * ) s_
               exit
            end if
!
            ix0 = i + 1_int32
!
         endif
      enddo
!
      if (present(year)) year=year_
      if (present(month)) month=month_
      if (present(day)) day=day_
!
      if (present(h)) h=h_
      if (present(m)) m=m_
      if (present(s)) s=s_
!
      return
!
   End subroutine str2time
!
! =========================================================================================


! =========================================================================================
!
   Subroutine str2epoch ( str, epoch )
!
      implicit none
!
!  dummy variables
!
      character(len=*), intent(in )  :: str
      integer(int64)  , intent(out)  :: epoch
!
!  local variables
!
      integer(int32)  :: year, month, day, h, m 
      real(double)    :: s
!
! ---
!
!.....get epoch
!
      call str2time ( str, year, month, day, h, m, s )
      call time2epoch( year, month, day, h, m, s, epoch )
!
      return
!
   End subroutine str2epoch
!
! =========================================================================================


! =========================================================================================
!
   Subroutine epoch2gmt( epoch, str )
!
      implicit none
!
!  dummy variables
!
      integer(int64)  , intent(in)   :: epoch
      character(len=*), intent(out)  :: str
!
!  local variables
!
      integer(int32)     :: year, month, day, h, m, s_
      real(double)       :: s, r_
      character(len=time_len)  :: str_
!
! ---
!
      call epoch2time ( epoch, year, month, day, h, m, s )
!
      s_ = int( s, int32 )
      r_ = s-s_
!
      write (str_,"(i4.4,'-',i2.2,'-',i2.2,'T',2(i2.2,':'),i2.2,f7.6)") &
         year, month, day, h, m, s_, r_
!
!.....trim
!
      str=str_(1:len(str))
!
      return
!
   End subroutine epoch2gmt
!
! =========================================================================================


! =========================================================================================
!
   Subroutine elaps2str( epoch, str )
!
      implicit none
!
!  dummy variables
!
      integer(int64)   , intent(in )  :: epoch
      character(len=23), intent(out)  :: str
!
!  local variables
!
      integer(int32)  :: h, m, s
      real(double)    :: time_s
!
! ---
!
!  epoch to seconds
!
      time_s = real( epoch, double ) / base_s2unit
!
!  hours, minutes, seconds
!
      h = int( time_s / 3600_int32, int32 )
      m = int( ( time_s - h * 3600_int32 ) / 60_int32, int32 )
      s = nint( time_s - h * 3600_int32 - m * 60_int32, int32 )
!
!.....write to string
!
      write ( str, "(i3,2(a,i2),a)" ) h, ' hours ', m, ' min ', s, ' sec'
!
      return
!
   End subroutine elaps2str
!
! =========================================================================================


! =========================================================================================
!
   Subroutine epoch2day ( epoch )
!
      implicit none
!
!.....dummy variables
!
      integer(int64), intent(inout)  :: epoch
!
!.....local variables
!
      double precision, parameter :: day = 86400._double * base_s2unit
!
! ---
!
!.....subtract entire days since epoch
!
      epoch = nint( epoch - floor( epoch / day ) * day, int64 ) 
!
      return
!
   End subroutine epoch2day
!
! =========================================================================================


! =========================================================================================
!
   Subroutine epochlist2day ( epoch )
!
      implicit none
!
!  dummy variables
!
      integer(int64), dimension(:), intent(inout)  :: epoch
!
!.....local variables
!
      real(double), parameter :: day = 86400._double * base_s2unit
!
! ---
!
!.....subtract entire days since epoch
!
      epoch = nint( epoch - floor( epoch / day ) * day, int64 )
!
      return
!
   End subroutine epochlist2day
!
! =========================================================================================


! =========================================================================================
!
   Function epoch_range_seconds ( epoch1, epoch2 ) result( diff )
!
      implicit none
!
!.....dummy variables
!
      integer(int64), intent(in)  :: epoch1, epoch2
      real(double)                :: diff
!
! ---
!
      diff = abs( real( epoch2 - epoch1, double ) ) / base_s2unit 
!
      return
!
   End function epoch_range_seconds
!
! =========================================================================================


! =========================================================================================
!
   Function epoch2seconds ( epoch ) result( s )
!
      implicit none
!
!.....dummy variables
!
      real(double)                :: s
      integer(int64), intent(in)  :: epoch
!
! ---
!
      s = real( epoch / base_s2unit, double ) 
!
      return
!
   End function epoch2seconds
!
! =========================================================================================


! =========================================================================================
!
   Function epoch2days ( epoch ) result( d )
!
      implicit none
!
!  dummy variables
!
      real(double)                :: d
      integer(int64), intent(in)  :: epoch
!
! ---
!
      d = real( epoch / base_s2unit, double ) / 86400._double
!
      return
!
   End function epoch2days
!
! =========================================================================================


! =========================================================================================
!
   Function seconds2epoch ( s ) result( epoch )
!
      implicit none
!
!  dummy variables
!
      real(double), intent(in)  :: s
      integer(int64)            :: epoch
!
! ---
!
      epoch = nint( s * time_scalar, int64 )
!
      return
!
   End function seconds2epoch
!
! =========================================================================================


! =========================================================================================
!
   Function epoch2julday ( epoch ) result( julday )
!
      implicit none
!
!  dummy variables
!
      integer(int64), intent(in)  :: epoch
      real(double)                :: julday
!
! ---
!
      julday = base_julday + epoch2days( epoch )
!
      return
!
   End function epoch2julday
!
! =========================================================================================


! =========================================================================================
!
   Subroutine fixepoch ( epoch, old_scalar )
!
      implicit none
!
!  dummy variables
!
      integer(int64), intent(inout)            :: epoch
      integer(int32), intent(in)   , optional  :: old_scalar
!
!   local variables
!
      integer(int64)  :: scalar
!
! ---
!
      if ( present(old_scalar) ) then
         scalar = base_s2unit / int(old_scalar,int64)
      else
         scalar = base_s2unit / 1000_int64 ! to fix mixed epoch in seconds and miliseconds
      end if
!
!.....check if epoch is smaller than 1 year (365.25*86400) -> wrong scalar!!
!
      if ( needepochfix(epoch) ) epoch = epoch*scalar
!
      return
!
   End subroutine fixepoch
!
! =========================================================================================


! =========================================================================================
!
   Function fixepochf ( epoch, old_scalar ) result( newepoch )
!
      implicit none
!
!.....dummy variables
!
      integer(int64), intent(in)            :: epoch
      integer(int32), intent(in), optional  :: old_scalar
      integer(int64)  :: newepoch
!
! ---
!
      newepoch = epoch
      call fixepoch( newepoch, old_scalar )
!
      return
!
   End function fixepochf
!
! =========================================================================================


! =========================================================================================
!
   Subroutine fixepochlist ( epoch, old_scalar )
!
      implicit none
!
!  dummy variables
!
      integer(int64), intent(inout), dimension(:)  :: epoch
      integer(int32), intent(in)   , optional      :: old_scalar
!
!   local variables
!
      integer(int64)  :: scalar
!
! ---
!
      if ( present(old_scalar) ) then
         scalar = base_s2unit / int(old_scalar,int64)
      else
         scalar = base_s2unit / 1000_int64 ! to fix mixed epoch in seconds and miliseconds
      end if
!
!.....check if epoch is smaller than 1 year (365.25*86400) -> wrong scalar!!
!
      if ( needepochfix(epoch(1)) ) epoch = epoch*scalar
!
      return
!
   End subroutine fixepochlist
!
! =========================================================================================


! =========================================================================================
!
   Function fixepochlistf ( epoch, old_scalar ) result( newepoch )
!
      implicit none
!
!.....dummy variables
!
      integer(int64), intent(in), dimension(:)  :: epoch
      integer(int32), intent(in), optional      :: old_scalar
      integer(int64), dimension(size(epoch,1))  :: newepoch
!
! ---
!
      newepoch = epoch
      call fixepochlist( newepoch, old_scalar )
!
      return
!
   End function fixepochlistf
!
! =========================================================================================


! =========================================================================================
!
   Function needepochfix ( epoch ) result( fix )
!
      implicit none
!
!  dummy variables
!
      logical                               :: fix
      integer(int64), intent(in)            :: epoch
!
!   local variables
!
      integer(int64)  :: check
!
! ---
!
!.....check if epoch is smaller than 60 days -> wrong scalar!!
!
      check = epoch-base_epoch
      fix = check .lt. 5184000_int64*base_s2unit
!
      return
!
   End function needepochfix
!
! =========================================================================================


End module time
