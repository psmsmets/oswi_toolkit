!*****************************************************************************80
!
!                           L I B O S W I _ F I L E N A M E
!
!  Module:       / (include)
!
!  Programmer:   Pieter S. M. Smets
!                R&D depart. of Seismology and Acoustics - Koninklijk Nederlands
!                Meteorologisch Instituut (KNMI)
!                De Bilt, The Netherlands
!
!  Date:         May 20, 2016
!
!  Language:     Fortran-90
!
!  Description:  Substitute filename variables
!
!
!*****************************************************************************80


!*****************************************************************************80
Subroutine liboswi_substitute_filename ( string, filename, hdr, epoch, frequency )
!*****************************************************************************80
!
! Subroutine liboswi_substitute_filename
!
! Description
!
!   Substitute filename variables, e.g., filename='%V__%C_%T_%R__%G__%Y%M%D_%H'
!
!   Filename variables:
!     %V  = output variable type
!     %A  = output variable standard name
!     %G  = output grid type
!     %C  = 2dfd data class
!     %T  = 2dfd data type
!     %Y  = year
!     %M  = month
!     %D  = day of month
!     %O  = day of year
!     %H  = hour
!     %S  = forecast step
!     %N  = ensemble number
!     %F  = frequency
!     %F0 = first frequency
!     %F1 = last frequency
!
!*****************************************************************************80
!
  implicit none
!
! Dummy variables
!
  character(len=*), intent(in)            :: string
  character(len=*), intent(out)           :: filename
  type(type_oswi_hdr), intent(in)         :: hdr
  integer(int64)  , intent(in)            :: epoch
  real(double)    , intent(in), optional  :: frequency
!
! Local variables
!
  character(len=len_trim(string))  :: pattern
  character(len=2)   :: var 
  integer(int32)     :: i, y, m, d, o, h
!
! Convert epoch
!
  call epoch2time ( epoch=epoch, year=y, month=m, day=d, h=h )
  call monthday2doy ( month=m, day=d, doy=o, year=y )
!
! Evaluate filename pattern
!
  i=0_int32
  pattern=trim(string)
  filename=''
!
  do while (.true.)
    i=scan(pattern,'%',.false.)
    if (i.eq.0_int32) then
      filename=trim(filename)//pattern(1_int32:len_trim(pattern))
      exit
    end if
    filename=trim(filename)//pattern(1_int32:i-1_int32)
    if (len_trim(pattern).lt.i+1_int32) exit
    var=pattern(i:i+1_int32)
    select case (strlowcase(var))
      case ('%v') ! type 
        write(filename,'(2a)') trim(filename), trim(hdr%type)
      case ('%a') ! standard_name
        write(filename,'(2a)') trim(filename), trim(hdr%standard_name)
      case ('%g') ! gridType
        write(filename,'(2a)') trim(filename), trim(hdr%grid%gridType)
      case ('%c') ! dataclass
        write(filename,'(2a)') trim(filename), trim(hdr%osw%dataClass)
      case ('%t') ! datatype
        write(filename,'(2a)') trim(filename), trim(hdr%osw%dataType)
      case ('%r') ! dataStream
        write(filename,'(2a)') trim(filename), trim(hdr%osw%dataStream)
      case ('%y') ! year
        write(filename,'(a,i4.4)') trim(filename), y
      case ('%m') ! month
        write(filename,'(a,i2.2)') trim(filename), m
      case ('%d') ! day
        write(filename,'(a,i2.2)') trim(filename), d
      case ('%o') ! doy
        write(filename,'(a,i3.3)') trim(filename), o
      case ('%h') ! hour
        write(filename,'(a,i2.2)') trim(filename), h
      case ('%e') ! epoch seconds
        write(filename,'(a,i10.10)') trim(filename), int(epoch/time_scalar,int32)
      case ('%s') ! forecast step
        write(filename,'(a,i3.3)') trim(filename), hdr%osw%forecastStep
      case ('%n') ! ensemble number
        write(filename,'(a,i2.2)') trim(filename), hdr%osw%ensembleNumber
      case ('%f') ! frequency
        if (present(frequency)) then
          write(filename,'(a,f7.5)') trim(filename), frequency
        else
          write(filename,'(a,f7.5)') trim(filename), hdr%f%dat(hdr%f%ixSel)
        end if
      case ('%0') ! first frequency
        write(filename,'(a,f7.5)') trim(filename), hdr%f%first
      case ('%1') ! last frequency
        write(filename,'(a,f7.5)') trim(filename), hdr%f%last
      case default
    end select
    if (i+2_int32.eq.len_trim(pattern)) exit
    pattern=pattern(i+2_int32:len_trim(pattern))
  end do
!
  return
!
!*****************************************************************************80
End subroutine liboswi_substitute_filename
!*****************************************************************************80
