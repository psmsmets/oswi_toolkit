Subroutine oswi_set_grid_from_grib ( oswi, grid )
!*****************************************************************************80
!
!! OSWI_SET_GRID_FROM_GRIB
!
!  Description:
!
!    Set oswi grid type. 
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_grid ) oswi, oswi grid type.
!
!    Input, type ( type_grib_grid ), grid, grib grid type
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid), intent(inout)  :: oswi
  type(type_grib_grid), intent(in   )  :: grid
!
  oswi%gridType = grid%gridType
  oswi%distinctGridValues = grid%distinctGridValues
  oswi%earthIsOblate = grid%earthIsOblate
  oswi%earthRadius = grid%earthRadius
  oswi%numberOfPoints = grid%numberOfPoints
  if (.not.grid%distinctGridValues) then
         allocate(oswi%pl(oswi%numberOfPoints))
     oswi%pl = grid%pl
  end if
  call oswi_set_grid_ll_from_grib ( oswi%lat, grid%lat )
  call oswi_set_grid_ll_from_grib ( oswi%lon, grid%lon )
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_grid_from_grib


Subroutine oswi_set_grid_ll_from_grib ( oswi, ll )
!*****************************************************************************80
!
!! OSWI_SET_GRID_LL_FROM_GRIB
!
!  Description:
!
!    Set oswi grid_ll type. 
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_grid_ll ) oswi, oswi grid_ll type.
!
!    Input, type ( type_grib_grid_ll ), ll, grib grid_ll type
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid_ll), intent(inout)  :: oswi
  type(type_grib_grid_ll)  , intent(in   )  :: ll
!
! ----
!
     oswi%reversed = ll%reversed
     oswi%size = ll%size
     oswi%count = ll%count
     oswi%ixFirst = ll%ixFirst
     oswi%ixLast = ll%ixLast
     oswi%first = ll%first
     oswi%last = ll%last
     oswi%increment = ll%increment
     oswi%range = ll%range
     if (allocated(ll%dat)) then
        allocate(oswi%dat(oswi%size))
    oswi%dat=ll%dat
     end if
     if (allocated(ll%mask)) then
        allocate(oswi%mask(oswi%size))
    oswi%mask=ll%mask
     end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_grid_ll_from_grib


Subroutine oswi_set_f_from_grib ( oswi, f )
!*****************************************************************************80
!
!! OSWI_SET_F_FROM_GRIB
!
!  Description:
!
!    Set oswi frequency type. 
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_f ) oswi, oswi frequency type.
!
!    Input, type ( type_grib_f ), f, grib frequency type
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_f), intent(inout)  :: oswi
  type(type_grib_frq), intent(in   )  :: f
!
! ----
!
  oswi%size=f%count
  oswi%count=oswi%size
  oswi%ixFirst=1_int32
  oswi%ixLast=oswi%size
  allocate( oswi%sound(oswi%size), oswi%wave(oswi%size), oswi%mask(oswi%size) )
  oswi%wave=pack(f%dat,f%mask)
  oswi%sound=pack(2*f%dat,f%mask)
  oswi%mask=.true.
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_f_from_grib


Subroutine oswi_set_osw_from_grib ( oswi, grib_2dfd )
!*****************************************************************************80
!
!! OSWI_SET_OSW
!
!  Description:
!
!    Set oswi wave type. 
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi ) oswi, oswi general type.
!
!    Input, type ( type_grib_2dfd ), grib_2dfd, grib 2dfd type
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi)   , intent(inout)  :: oswi
  type(type_grib_2dfd), intent(in   )  :: grib_2dfd
!
! ----
!
  oswi%osw%shortName = grib_2dfd%shortName
  oswi%osw%units = grib_2dfd%units
  oswi%osw%centre = grib_2dfd%centre
  oswi%osw%dataClass = grib_2dfd%dataClass
  oswi%osw%dataType = grib_2dfd%dataType
  oswi%osw%dataStream = grib_2dfd%dataStream
  oswi%osw%gridType = grib_2dfd%grid%gridType
  oswi%osw%paramId = grib_2dfd%paramId
  oswi%osw%experimentVersionNumber = grib_2dfd%experimentVersionNumber
  oswi%osw%editionNumber = grib_2dfd%editionNumber
  oswi%osw%frequencies = grib_2dfd%frequency%size
  oswi%osw%directions = grib_2dfd%direction%size
  oswi%osw%directionIncrement = grib_2dfd%direction%increment
  oswi%osw%directionFullCircle = grib_2dfd%direction%fullCircle
!
  oswi%missingValue=-9999_int32
!
  call oswi_set_grid_from_grib( oswi%grid, grib_2dfd%grid )
  call oswi_set_f_from_grib( oswi%f, grib_2dfd%frequency )
!
  allocate( &
    oswi%dat_llf(oswi%grid%numberOfPoints,oswi%f%size), &
    oswi%dat_ll(oswi%grid%numberOfPoints) &
  )
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_osw_from_grib
