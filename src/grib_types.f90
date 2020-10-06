!*****************************************************************************80
!
!                             G R I B _ T Y P E S
!
!  Module:       grib_types
!
!  Programmer:   Pieter S. M. Smets
!                R&D depart. of Seismology and Acoustics - Koninklijk Nederlands
!                Meteorologisch Instituut (KNMI)
!                De Bilt, The Netherlands
!
!  Date:         August 2, 2017
!
!  Language:     Fortran-90
!
!  Description:  Define ecmwf hybrid grib types
!
!
!*****************************************************************************80

Module grib_types

  use io_types
  use time
  use eccodes
  use iso_c_binding, only: C_LOC, C_F_POINTER
  implicit none
!
! parameters
!
   integer(int32), parameter, public  :: grib_alen = 80_int32   ! attribute length
   integer(int32), parameter, public  :: grib_dlen = 2048_int32 ! description length
   real(double)  , parameter, private :: grib_res  = 1e-6
!
! types
!
  type type_grib_mars
    logical                  :: isECMWF
    integer(int32)           :: experimentVersionNumber, editionNumber
    character(len=grib_alen) :: centre, dataClass, dataType, dataStream
  end type type_grib_mars
!
  type type_grib_grid_ll
    logical                  :: reversed 
    integer(int32)           :: size, count, ixFirst, ixLast
    real(double)             :: first, last, increment, range
    real(double), dimension(:), allocatable  :: dat, dat_unpacked
    logical     , dimension(:), allocatable  :: mask 
  end type type_grib_grid_ll

  type type_grib_grid ! geo, geography
    character(len=grib_alen) :: gridName, gridType, packingType
    logical                  :: isOctahedral, distinctGridValues, earthIsOblate
!   logical                  :: iScansNegatively, jScansPositively, jPointsAreConsecutive
    integer(int32)           :: numberOfPoints, shapeOfTheEarth, earthRadius
    integer(int32), dimension(:), allocatable  :: pl !  number of points along a full parallel
    type(type_grib_grid_ll)  :: lon, lat
  end type type_grib_grid

  type type_grib_num
    logical                  :: defined 
    integer(int32)           :: count, size, sel
    logical       , dimension(:), allocatable :: mask
    integer(int32), dimension(:), allocatable :: list
  end type type_grib_num

  type type_grib_step
    logical                  :: defined
    character(len=grib_alen) :: units
    integer(int32)           :: count, size, sel, startStep, endStep, stepRange
    logical       , dimension(:), allocatable :: mask
    integer(int32), dimension(:), allocatable :: list
  end type type_grib_step

  type type_grib_vert ! vertical
    logical                  :: defined, PVPresent 
    character(len=grib_alen) :: typeOfLevel
    integer(int32)           :: size, count, numberOfPv
    real(double)  , dimension(:), allocatable :: pv
    real(double)  , dimension(:), pointer     :: a, b
    integer(int32), dimension(:), allocatable :: list
    logical       , dimension(:), allocatable :: mask
    integer(int32), dimension(:), pointer     :: list_flip
    logical       , dimension(:), pointer     :: mask_flip
  end type type_grib_vert

  type type_grib_dir
    logical                  :: defined, fullCircle, linearIncrement
    integer(int32)           :: size, scalingFactor
    integer(int32), pointer  :: count
    real(double)             :: increment 
    integer(int32), dimension(:), allocatable :: list
    real(double)  , dimension(:), allocatable :: dat
  end type type_grib_dir

  type type_grib_frq
    logical                  :: defined 
    integer(int32)           :: size, count, scalingFactor, ixFirst, ixLast
    integer(int32), dimension(:), allocatable  :: list
    real(double)  , dimension(:), allocatable  :: dat
    logical       , dimension(:), allocatable  :: mask
  end type type_grib_frq

  type type_grib_varSurf
    logical                                         :: defined, surface
    character(len=grib_alen)                        :: shortName, longName, standard_name, units, typeOfLevel
    integer(int32)                                  :: paramIndex, paramId, missingValue
    real(single), dimension(:,:,:)  , allocatable   :: dat
    real(single), dimension(:,:,:,:), pointer       :: regular_ll
    real(single), dimension(:,:,:,:), pointer       :: regular_ll_flip
  end type type_grib_varSurf

  type type_grib_varLevel
    logical                                         :: defined, level
    character(len=grib_alen)                        :: shortName, longName, standard_name, units, typeOfLevel
    integer(int32)                                  :: paramIndex, paramId, missingValue
    real(single), dimension(:,:,:,:)  , allocatable :: dat ! (ll, level, step, ens)
    real(single), dimension(:,:,:,:)  , pointer     :: dat_flip
    real(single), dimension(:,:,:,:,:), pointer     :: regular_ll
    real(single), dimension(:,:,:,:,:), pointer     :: regular_ll_flip 
  end type type_grib_varLevel

! Fortran arrays are stored in column-major order!

Contains

!*****************************************************************************80
!
! Function isgrib 
!
!   Check if file is of grib type
!
!****************************
  Logical function isgrib ( gribFile )
!****************************
!
   implicit none
!
!   Dummy variables
!
   character(len=*), intent(in) :: gribFile
!
!   Local variables
!
   integer(int32)  :: ifile, iret
!
!  ---
!
!   Open the grib file
!
   call codes_open_file ( ifile, gribFile, 'r', iret )
   isgrib = iret.eq.0_int32
   if (isgrib) call codes_close_file( ifile )
!
   return
!
  End function isgrib
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_clear_type_mars
!
!   Initialize grib grid latlon structure.
!
!****************************
  Subroutine grib_clear_type_mars ( mars )
!****************************
!
    implicit none
!
!...Dummy variables
!
    type(type_grib_mars), intent(inout), target  :: mars
!
!  ---
!
    mars%isECMWF = .false.
    mars%experimentVersionNumber = 0_int32
    mars%editionNumber = 0_int32
    mars%centre = ''
    mars%dataClass = ''
    mars%dataType = ''
    mars%dataStream = ''
!
    return
!
  End subroutine grib_clear_type_mars
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_compare_type_mars
!
!   Compare grib mars structure.
!
!****************************
  function grib_compare_type_mars ( marsA, marsB ) result ( match )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_mars), intent(in)  :: marsA, marsB
    logical :: match
!
!  ---
!
    match=.false.
!
    if (marsA%isECMWF.neqv.marsB%isECMWF) return
    if (marsA%experimentVersionNumber.ne.marsB%experimentVersionNumber) return
    if (marsA%editionNumber.ne.marsB%editionNumber) return
    if (marsA%centre.ne.marsB%centre) return
    if (marsA%dataClass.ne.marsB%dataClass) return
    if (marsA%dataType.ne.marsB%dataType) return
    if (marsA%dataStream.ne.marsB%dataStream) return
!
    match=.true.
!
    return
!
  End function grib_compare_type_mars
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_clear_type_grid_ll
!
!   Initialize grib grid latlon structure.
!
!****************************
  Subroutine grib_clear_type_grid_ll ( ll )
!****************************
!
    implicit none
!
!...Dummy variables
!
    type(type_grib_grid_ll), intent(inout), target  :: ll
!
!  ---
!
    ll%reversed = .false.
    ll%size = 0_int32
    ll%count = 0_int32
    ll%ixFirst = -1_int32
    ll%ixLast = -1_int32
    ll%first = 0._double
    ll%last = 0._double
    ll%increment = 0._double
    ll%range = 0._double
    if (allocated(ll%dat)) deallocate(ll%dat)
    if (allocated(ll%dat_unpacked)) deallocate(ll%dat_unpacked)
    if (allocated(ll%mask)) deallocate(ll%mask)
!
    return
!
  End subroutine grib_clear_type_grid_ll
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_compare_type_grid_ll
!
!   Compare grib grid structures.
!
!****************************
  function grib_compare_type_grid_ll ( llA, llB ) result ( match )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_grid_ll), intent(in)  :: llA, llB
    logical :: match
!
!  ---
!
    match=.false.
!
    if (llA%reversed.neqv.llB%reversed) return
    if (llA%size.ne.llB%size) return
    if (abs(llA%first-llB%first).gt.grib_res) return
    if (abs(llA%last-llB%last).gt.grib_res) return
    if (abs(llA%increment-llB%increment).gt.grib_res) return
    if (abs(llA%range-llB%range).gt.grib_res) return
!
    match=.true.
!
    return
!
  End function grib_compare_type_grid_ll
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_clear_type_grid_grid
!
!   Initialize grib grid structure.
!
!****************************
  Subroutine grib_clear_type_grid ( grid )
!****************************
!
    implicit none
!
!...Dummy variables
!
    type(type_grib_grid), intent(inout), target  :: grid
!
!  ---
!
    call grib_clear_type_grid_ll( grid%lat )
    call grib_clear_type_grid_ll( grid%lon )
    grid%gridName = ''
    grid%gridType = ''
    grid%packingType = ''
    grid%isOctahedral = .false.
    grid%distinctGridValues = .false.
    grid%earthIsOblate = .false.
    grid%numberOfPoints = 0_int32
    grid%shapeOfTheEarth = 0_int32
    grid%earthRadius = 0_int32
    if (allocated(grid%pl)) deallocate(grid%pl)
!
    return
!
  End subroutine grib_clear_type_grid
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_compare_type_grid
!
!   Compare grib grid structures.
!
!****************************
  function grib_compare_type_grid ( gridA, gridB ) result ( match )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_grid), intent(in)  :: gridA, gridB
    logical :: match
!
!  ---
!
    match=.false.
!
    if (gridA%isOctahedral.neqv.gridB%isOctahedral) return
    if (gridA%distinctGridValues.neqv.gridB%distinctGridValues) return
    if (gridA%gridName.ne.gridB%gridName) return
    if (gridA%gridType.ne.gridB%gridType) return
    if (gridA%packingType.ne.gridB%packingType) return
    if (gridA%numberOfPoints.ne.gridB%numberOfPoints) return
    if (.not.grib_compare_type_grid_ll( gridA%lat, gridB%lat )) return
    if (.not.grib_compare_type_grid_ll( gridA%lon, gridB%lon )) return
!
    match=.true.
!
    return
!
  End function grib_compare_type_grid
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_allocate_type_grid
!
!   Allocate grib grid latlon structure.
!
!****************************
  Subroutine grib_allocate_type_grid ( grid )
!****************************
!
    implicit none
!
!...Dummy variables
!
    type(type_grib_grid), intent(inout), target  :: grid
!
!  ---
!
    grid%distinctGridValues = grid%gridType.eq.'regular_ll'
!
!   lat
!
    if (allocated(grid%lat%dat).and.size(grid%lat%dat,1).ne.grid%lat%size) &
      deallocate(grid%lat%dat)
    if (.not.allocated(grid%lat%dat)) allocate(grid%lat%dat(grid%lat%size))
!
    if (allocated(grid%lat%mask).and.size(grid%lat%mask,1).ne.grid%lat%size) &
      deallocate(grid%lat%mask)
    if (.not.allocated(grid%lat%mask)) allocate(grid%lat%mask(grid%lat%size))
    grid%lat%mask=.true.
!
!   lon
!
    if (grid%distinctGridValues) then
!
      if (allocated(grid%lon%dat).and.size(grid%lon%dat,1).ne.grid%lon%size) &
        deallocate(grid%lon%dat)
      if (.not.allocated(grid%lon%dat)) allocate(grid%lon%dat(grid%lon%size))
!
      if (allocated(grid%lon%mask).and.size(grid%lon%mask,1).ne.grid%lon%size) &
        deallocate(grid%lon%mask)
      if (.not.allocated(grid%lon%mask)) allocate(grid%lon%mask(grid%lon%size))

      if (allocated(grid%pl)) deallocate( grid%pl )
!
    else
!
      if (allocated(grid%lon%dat).and.size(grid%lon%dat,1).ne.grid%numberOfPoints) &
        deallocate(grid%lon%dat)
      if (.not.allocated(grid%lon%dat)) allocate(grid%lon%dat(grid%numberOfPoints))
!
      if (allocated(grid%lon%mask).and.size(grid%lon%mask,1).ne.grid%numberOfPoints) &
        deallocate(grid%lon%mask)
      if (.not.allocated(grid%lon%mask)) allocate(grid%lon%mask(grid%numberOfPoints))
!
      if (allocated(grid%pl).and.size(grid%pl,1).ne.grid%lat%size) &
        deallocate(grid%pl)
      if (.not.allocated(grid%pl)) allocate(grid%pl(grid%lat%size))
!
      if (grid%gridType.eq.'reduced_ll') then
        if (allocated(grid%lat%dat_unpacked).and.size(grid%lat%dat_unpacked,1).ne.grid%numberOfPoints) &
          deallocate(grid%lat%dat_unpacked)
        if (.not.allocated(grid%lat%dat_unpacked)) allocate(grid%lat%dat_unpacked(grid%numberOfPoints))
      end if
!
    end if
!
    grid%lon%mask=.true.
!
    return
!
  End subroutine grib_allocate_type_grid
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_clear_type_num
!
!   Initialize grib ensemble number structure.
!
!****************************
  Subroutine grib_clear_type_num ( number )
!****************************
!
    implicit none
!
!...Dummy variables
!
    type(type_grib_num), intent(inout), target  :: number
!
!  ---
!
    number%defined = .false.
    number%size = 0_int32
    number%count = 0_int32 
    number%sel = 1_int32
    if (allocated(number%mask)) deallocate(number%mask)
    if (allocated(number%list)) deallocate(number%list)
!
    return
!
  End subroutine grib_clear_type_num
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_compare_type_num
!
!   Compare grib number structures.
!
!****************************
  function grib_compare_type_num ( numberA, numberB ) result ( match )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_num), intent(in)  :: numberA, numberB
    logical :: match
!
!  ---
!
    match=.false.
!
    if (numberA%defined.neqv.numberB%defined) return
    if (numberA%size.ne.numberB%size) return
    if (numberA%count.ne.numberB%count) return
    if (numberA%sel.ne.numberB%sel) return
    if (.not.all(numberA%mask.eqv.numberB%mask)) return
    if (.not.all(numberA%list.eq.numberB%list)) return
!
    match=.true.
!
    return
!
  End function grib_compare_type_num
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_allocate_type_num
!
!   Allocate grib number structure.
!
!****************************
  Subroutine grib_allocate_type_num ( number )
!****************************
!
    implicit none
!
!...Dummy variables
!
    type(type_grib_num), intent(inout), target  :: number
!
!  ---
!
    if (.not.number%defined) number%size=1_int32
    number%count=number%size
!
!   list
!
    if (allocated(number%list).and.size(number%list,1).ne.number%size) &
      deallocate(number%list)
    if (.not.allocated(number%list)) allocate(number%list(number%size))
!
!   mask
!
    if (allocated(number%mask).and.size(number%mask,1).ne.number%size) &
      deallocate(number%mask)
    if (.not.allocated(number%mask)) allocate(number%mask(number%size))
    number%mask=.true.
!
    return
!
  End subroutine grib_allocate_type_num
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_clear_type_step
!
!   Initialize grib forecast step structure.
!
!****************************
  Subroutine grib_clear_type_step ( step )
!****************************
!
    implicit none
!
!...Dummy variables
!
    type(type_grib_step), intent(inout), target  :: step
!
!  ---
!
    step%defined = .false.
    step%size = 0_int32
    step%count = 0_int32
    step%sel = 1_int32
    step%startStep = 0_int32
    step%endStep = 0_int32
    step%stepRange = 0_int32
    if (allocated(step%mask)) deallocate(step%mask)
    if (allocated(step%list)) deallocate(step%list)
!
    return
!
  End subroutine grib_clear_type_step
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_compare_type_step
!
!   Compare grib step structures.
!
!****************************
  function grib_compare_type_step ( stepA, stepB ) result ( match )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_step), intent(in)  :: stepA, stepB
    logical :: match
!
!  ---
!
    match=.false.
!
    if (stepA%defined.neqv.stepB%defined) return
    if (stepA%size.ne.stepB%size) return
    if (stepA%startStep.ne.stepB%startStep) return
    if (stepA%endStep.ne.stepB%endStep) return
    if (stepA%stepRange.ne.stepB%stepRange) return
    if (stepA%sel.ne.stepB%sel) return
    if (.not.all(stepA%mask.eqv.stepB%mask)) return
    if (.not.all( stepA%list.eq.stepB%list)) return
!
    match=.false.
!
    return
!
  End function grib_compare_type_step
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_allocate_type_step
!
!   Allocate grib number structure.
!
!****************************
  Subroutine grib_allocate_type_step ( step )
!****************************
!
    implicit none
!
!...Dummy variables
!
    type(type_grib_step), intent(inout), target  :: step
!
!  ---
!
!   list
!
    if (allocated(step%list).and.size(step%list,1).ne.step%size) &
      deallocate(step%list)
    if (.not.allocated(step%list)) allocate(step%list(step%size))
!
!   mask
!
    if (allocated(step%mask).and.size(step%mask,1).ne.step%size) &
      deallocate(step%mask)
    if (.not.allocated(step%mask)) allocate(step%mask(step%size))
    step%mask=.true.
!
    step%count=step%size
!
    return
!
  End subroutine grib_allocate_type_step
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_get_type_step
!
!   Get grib step structures from grib file.
!
!****************************
  Subroutine grib_get_type_step ( igrib, step, error, verbose )
!****************************
!
    implicit none
!
!   Dummy variables
!
    integer(int32)      , intent(in)     :: igrib
    type(type_grib_step), intent(inout)  :: step
    logical             , intent(in)     :: verbose
    integer(int32)      , intent(out)    :: error
!
!  ---
!
    if (verbose) print "(6x,a)", '>> Get forecast steps'
!
    call codes_get( igrib, 'stepUnits', step%units, error )
    call codes_get( igrib, 'stepRange', step%stepRange, error )
    call codes_get( igrib, 'startStep', step%startStep, error )
    call codes_get( igrib, 'endStep', step%endStep, error )
!
    return
!
  End subroutine grib_get_type_step
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_clear_type_vert
!
!   Initialize grib frequency structure.
!
!****************************
  Subroutine grib_clear_type_vert ( vertical )
!****************************
!
    implicit none
!
!...Dummy variables
!
    type(type_grib_vert), intent(inout), target  :: vertical
!
!  ---
!
    vertical%defined = .false.
    vertical%PVPresent = .false.
    vertical%typeOfLevel = ''
    vertical%size = 0_int32
    vertical%count = 0_int32
    vertical%numberOfPV = 0_int32
    if (allocated(vertical%list)) deallocate(vertical%list)
    if (allocated(vertical%mask)) deallocate(vertical%mask)
    if (allocated(vertical%pv)) deallocate(vertical%pv)
    if (associated(vertical%a)) nullify(vertical%a)
    if (associated(vertical%b)) nullify(vertical%b)
    if (associated(vertical%list_flip)) nullify(vertical%list_flip)
    if (associated(vertical%mask_flip)) nullify(vertical%mask_flip)
!
    return
!
  End subroutine grib_clear_type_vert
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_compare_type_vert
!
!   Compare grib vertical structures.
!
!****************************
  function grib_compare_type_vert ( levelA, levelB ) result ( match )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_vert), intent(in)  :: levelA, levelB
    logical :: match
!
!  ---
!
    match=.false.
!
    if (levelA%defined.neqv.levelB%defined) return
    if (levelA%typeOfLevel.ne.levelB%typeOfLevel) return
    if (levelA%size.ne.levelB%size) return
    if (.not.all(levelA%mask.eqv.levelB%mask)) return
!
    match=.true.
!
    return
!
  End function grib_compare_type_vert
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_allocate_type_vert
!
!   Allocate grib vertical structure.
!
!****************************
  Subroutine grib_allocate_type_vert ( vertical )
!****************************
!
    implicit none
!
!...Dummy variables
!
    type(type_grib_vert), intent(inout), target  :: vertical
!
!  ---
!
!   list
!
    if (allocated(vertical%list).and.size(vertical%list,1).ne.vertical%size) deallocate(vertical%list)
    if (.not.allocated(vertical%list)) allocate(vertical%list(vertical%size))
!
!   Flip list
!
    vertical%list_flip=>vertical%list(vertical%size:1_int32:-1_int32)
!
!   mask
!
    if (allocated(vertical%mask).and.size(vertical%mask,1).ne.vertical%size) deallocate(vertical%mask)
    if (.not.allocated(vertical%mask)) allocate(vertical%mask(vertical%size))
    vertical%mask=.true.
!
!   flip mask
!
    vertical%mask_flip=>vertical%mask(vertical%size:1_int32:-1_int32)
!
!   pv
!
    if (.not.vertical%PVPresent) return
!
    if (allocated(vertical%pv).and.size(vertical%pv,1).ne.vertical%numberOfPV) deallocate(vertical%pv)
    if (.not.allocated(vertical%pv)) allocate(vertical%pv(vertical%numberOfPV))
!
!   a and b half-vertical coefficients
!
    vertical%a(0_int32:vertical%size)=>vertical%pv(1:vertical%size+1_int32)
    vertical%b(0_int32:vertical%size)=>vertical%pv(vertical%size+2_int32:vertical%numberOfPV)
!
    return
!
  End subroutine grib_allocate_type_vert
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_clear_type_frq
!
!   Initialize grib frequency structure.
!
!****************************
  Subroutine grib_clear_type_frq ( frequency )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_grib_frq), intent(inout), target  :: frequency
!
!  ---
!
    frequency%defined = .false.
    frequency%size = 0_int32
    frequency%count = 0_int32
    frequency%scalingFactor = 1_int32
    frequency%ixFirst = -1_int32
    frequency%ixLast = -1_int32
    if (allocated(frequency%list)) deallocate(frequency%list)
    if (allocated(frequency%dat)) deallocate(frequency%dat)
    if (allocated(frequency%mask)) deallocate(frequency%mask)
!
    return
!
  End subroutine grib_clear_type_frq
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_compare_type_frq
!
!   Compare grib frequency structures.
!
!****************************
  function grib_compare_type_frq ( frequencyA, frequencyB ) result ( match )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_frq), intent(in)  :: frequencyA, frequencyB
    logical :: match
!
!  ---
!
    match=.false.
!
    if (frequencyA%size.ne.frequencyB%size) return
    if (frequencyA%count.ne.frequencyB%count) return
    if (frequencyA%scalingFactor.ne.frequencyB%scalingFactor) return
    if (frequencyA%ixFirst.ne.frequencyB%ixFirst) return
    if (frequencyA%ixLast.ne.frequencyB%ixLast) return
    if (.not.all(frequencyA%list.eq.frequencyB%list)) return
    if (.not.all(abs(frequencyA%dat-frequencyB%dat).lt.grib_res)) return
!
    match=.false.
!
    return
!
  End function grib_compare_type_frq
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_clear_type_dir
!
!   Initialize grib direction structure.
!
!****************************
  Subroutine grib_clear_type_dir ( direction )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_grib_dir), intent(inout), target  :: direction
!
!  ---
!
    direction%defined = .false.
    direction%fullCircle = .false.
    direction%linearIncrement = .false.
    direction%size = 0_int32
    direction%count => direction%size
    direction%scalingFactor = 1_int32
    direction%increment = 0._double
    if (allocated(direction%list)) deallocate(direction%list)
    if (allocated(direction%dat)) deallocate(direction%dat)
!
    return
!
  End subroutine grib_clear_type_dir
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_compare_type_dir
!
!   Compare grib direction structures.
!
!****************************
  function grib_compare_type_dir ( directionA, directionB ) result ( match )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_dir), intent(in)  :: directionA, directionB
    logical :: match
!
!  ---
!
    match=.false.
!
    if (directionA%fullCircle.neqv.directionB%fullCircle) return
    if (directionA%linearIncrement.neqv.directionB%linearIncrement) return
    if (directionA%size.ne.directionB%size) return
    if (directionA%scalingFactor.ne.directionB%scalingFactor) return
    if (abs(directionA%increment-directionB%increment).gt.grib_res) return
    if (.not.all(directionA%list.eq.directionB%list)) return
    if (.not.all(abs(directionA%dat-directionB%dat).lt.grib_res)) return
!
    match=.true.
!
    return
!
  End function grib_compare_type_dir
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_allocate_type_varLevel
!
!   Allocate varLevel structure.
!
!****************************
  Subroutine grib_allocate_type_varLevel ( varLevel, points, levels, steps, numbers )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_varLevel), intent(inout), target  :: varLevel
    integer(int32)         , intent(in)              :: points, levels, steps, numbers
!
!  ---
!
    if (.not.varLevel%defined) return
!
    if ( allocated(varLevel%dat).and.( size(varLevel%dat,1).ne.points &
      .or. size(varLevel%dat,2).ne.levels .or. size(varLevel%dat,3).ne.steps &
      .or. size(varLevel%dat,4).ne.numbers ) ) deallocate(varLevel%dat)
    if (.not.allocated(varLevel%dat)) allocate(varLevel%dat(points,levels,steps,numbers))
    varLevel%dat = varLevel%missingValue
!
    return
!
  End subroutine grib_allocate_type_varLevel
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_reshape_type_varLevel
!
!   Reshape varLevel structure.
!
!****************************
  Subroutine grib_reshape_type_varLevel ( varLevel, latitudes, longitudes )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_varLevel), intent(inout), target  :: varLevel
    integer(int32)          , intent(in)             :: latitudes, longitudes
!
!  ---
!
    if (.not.varLevel%defined) return
!
    varLevel%dat_flip => varLevel%dat(:,size(varLevel%dat,2):1_int32:-1_int32,:,:)
!
    varLevel%regular_ll => grib_reshape_varLevel( varLevel%dat, &
        (/longitudes,latitudes,size(varLevel%dat,2),size(varLevel%dat,3),size(varLevel%dat,4)/) )
!
    varLevel%regular_ll_flip => varLevel%regular_ll(:,latitudes:1_int32:-1_int32,&
      size(varLevel%dat,2):1_int32:-1_int32,:,:)
!
    return
!
  End subroutine grib_reshape_type_varLevel
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_clear_type_varLevel
!
!   Clear varLevel structure.
!
!****************************
  Subroutine grib_clear_type_varLevel ( varLevel )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_varLevel), intent(inout), target  :: varLevel
!
!  ---
!
    varLevel%defined = .false.
    varLevel%shortName = ''
    varLevel%longName = ''
    varLevel%units = ''
    varLevel%typeOfLevel = ''
    varLevel%paramIndex = 0_int32
    varLevel%paramId = 0_int32
    varLevel%missingValue = 9999_int32
    if (allocated(varLevel%dat)) deallocate(varLevel%dat)
    if (associated(varLevel%dat_flip)) nullify(varLevel%dat_flip)
    if (associated(varLevel%regular_ll)) nullify(varLevel%regular_ll)
    if (associated(varLevel%regular_ll_flip)) nullify(varLevel%regular_ll_flip)
!
    return
!
  End subroutine grib_clear_type_varLevel
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_compare_type_varLevel
!
!   Compare varLevel structures.
!
!****************************
  function grib_compare_type_varLevel ( varLevelA, varLevelB ) result ( match )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_varLevel), intent(in)  :: varLevelA, varLevelB
    logical :: match
!
!  ---
!
    match=.false.
!
    if (varLevelA%defined.neqv.varLevelB%defined) return
    if (varLevelA%shortName.ne.varLevelB%shortName) return
    if (varLevelA%longName.ne.varLevelB%longName) return
    if (varLevelA%units.ne.varLevelB%units) return
    if (varLevelA%typeOfLevel.ne.varLevelB%typeOfLevel) return
    if (varLevelA%paramId.ne.varLevelB%paramId) return
    if (varLevelA%missingValue.ne.varLevelB%missingValue) return
!
    match=.true.
!
    return
!
  End function grib_compare_type_varLevel
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_allocate_type_varSurf
!
!   Allocate varSurf structure.
!
!****************************
  Subroutine grib_allocate_type_varSurf ( varSurf, points, steps, numbers )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_varSurf), intent(inout), target  :: varSurf
    integer(int32)         , intent(in)             :: points, steps, numbers
!
!  ---
!
    if (.not.varSurf%defined) return
!
    if ( allocated(varSurf%dat).and.( size(varSurf%dat,1).ne.points .or. size(varSurf%dat,2).ne.steps &
      .or. size(varSurf%dat,3).ne.numbers ) ) deallocate(varSurf%dat)
    if (.not.allocated(varSurf%dat)) allocate(varSurf%dat(points,steps,numbers))
    varSurf%dat = varSurf%missingValue
!
    return
!
  End subroutine grib_allocate_type_varSurf
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_reshape_type_varSurf
!
!   Reshape varSurf structure.
!
!****************************
  Subroutine grib_reshape_type_varSurf ( varSurf, latitudes, longitudes )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_varSurf), intent(inout), target  :: varSurf
    integer(int32)         , intent(in)             :: latitudes, longitudes
!
!  ---
!
    if (.not.varSurf%defined) return
!
    varSurf%regular_ll => grib_reshape_varSurf( varSurf%dat, &
        (/longitudes,latitudes,size(varSurf%dat,2),size(varSurf%dat,3)/) )
    varSurf%regular_ll_flip => varSurf%regular_ll(:,latitudes:-1_int32:1_int32,:,:)
!
    return
!
  End subroutine grib_reshape_type_varSurf
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_clear_type_varSurf
!
!   Clear varSurf structure.
!
!****************************
  Subroutine grib_clear_type_varSurf ( varSurf )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_varSurf), intent(inout), target  :: varSurf
!
!  ---
!
    varSurf%defined = .false.
    varSurf%shortName = ''
    varSurf%longName = ''
    varSurf%units = ''
    varSurf%typeOfLevel = ''
    varSurf%paramIndex = 0_int32
    varSurf%paramId = 0_int32
    varSurf%missingValue = 9999_int32
    if (allocated(varSurf%dat)) deallocate(varSurf%dat)
    if (associated(varSurf%regular_ll)) nullify(varSurf%regular_ll)
    if (associated(varSurf%regular_ll_flip)) nullify(varSurf%regular_ll_flip)
!
    return
!
  End subroutine grib_clear_type_varSurf
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_compare_type_varSurf
!
!   Compare varSurf structure.
!
!****************************
  function grib_compare_type_varSurf ( varSurfA, varSurfB ) result ( match )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_varSurf), intent(in)  :: varSurfA, varSurfB
    logical :: match
!
!  ---
!
    match=.false.
!
    if (varSurfA%defined.neqv.varSurfB%defined) return
    if (varSurfA%shortName.ne.varSurfB%shortName) return
    if (varSurfA%longName.ne.varSurfB%longName) return
    if (varSurfA%units.ne.varSurfB%units) return
    if (varSurfA%typeOfLevel.ne.varSurfB%typeOfLevel) return
    if (varSurfA%paramId.ne.varSurfB%paramId) return
    if (varSurfA%missingValue.ne.varSurfB%missingValue) return
!
    match=.true.
!
    return
!
  End function grib_compare_type_varSurf
!
!*****************************************************************************80


!*****************************************************************************80
!
! Subroutine grib_referenceTime
!
!   Get time from grib file.
!
!****************************
  Subroutine grib_referenceTime ( gribFile, epoch, timestring, mask )
!****************************
!
    implicit none
!
!   Dummy variables
!
    character(len=*), intent(in)             :: gribFile
    integer(int64)  , intent(out)            :: epoch
    character(len=*), intent(out), optional  :: timestring 
    logical         , intent(out), optional  :: mask 
!
!   Local variables
!
    logical           :: stopOnError, exists
    integer(int32)    :: ifile, igrib, iret, d, t
    character(len=4)  :: t_str
    character(len=8)  :: d_str
    character(len=23) :: tstr
!
!  ---
!
!   Init
!
    if (present(mask)) then 
      stopOnError=.false.
      mask=.false.
    else
      stopOnError=.true.
    end if
    epoch = 0_int64
    if (present(timestring)) timestring=''
!
!   Open profile, if exists, stop otherwise.
!
    inquire( file = gribFile, exist = exists )
    if (.not.exists) then
      if (stopOnError) stop 'Error @ grib_referenceTime : grib file does not exist'
      return
    end if
!
!   Open file and load first data
!
    call codes_open_file ( ifile, gribFile, 'r', iret )
    if (iret.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_referenceTime : could not open file'
      return
    end if
!
    call codes_new_from_file ( ifile, igrib, iret )
    if (iret.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_referenceTime : could not get new from file'
      return
    end if
!
!   Get date and time from grib file.
!
    call codes_get( igrib, 'dataDate', d, iret )
    if (iret.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_referenceTime : could not get dataDate'
      return
    end if
    call codes_get( igrib, 'dataTime', t, iret )
    if (iret.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_referenceTime : could not get dataTime'
      return
    end if
!
    write (d_str,"(i8)"  ) d
    write (t_str,"(i4.4)") t
!
    tstr = d_str(1:4)//'/'//d_str(5:6)//'/'//d_str(7:)//' '//t_str(1:2)//':'//t_str(3:)//':00.000'
    call str2epoch ( tstr, epoch )
!
!   Release grib and close file.
!
    call codes_release( igrib )
    call codes_close_file( ifile )
!
    if (present(timestring)) call epoch2str( epoch, timestring )
    if (present(mask)) mask=.true.
!
    return
!
  End subroutine grib_referenceTime
!
!*****************************************************************************80



!*****************************************************************************80
!
! Subroutine grib_get_epoch
!
!   Get time from grib file.
!
!****************************
  Subroutine grib_get_epoch ( igrib, epoch, timestring, error )
!****************************
!
    implicit none
!
!   Dummy variables
!
    integer(int32)  , intent(in)             :: igrib
    integer(int64)  , intent(out)            :: epoch
    character(len=*), intent(out), optional  :: timestring 
    integer(int32)  , intent(out), optional  :: error 
!
!   Local variables
!
    integer(int32)    :: iret, d, t
    character(len=4)  :: t_str
    character(len=8)  :: d_str
    character(len=23) :: tstr
!
!  ---
!
!   Init
!
    epoch = 0_int64
    if (present(timestring)) timestring=''
!
!   Get date and time from grib file.
!
    call codes_get( igrib, 'dataDate', d, iret )
    if (iret.ne.0_int32) then
      if (present(error)) error=iret
      return
    end if
    call codes_get( igrib, 'dataTime', t, iret )
    if (iret.ne.0_int32) then
      if (present(error)) error=iret
      return
    end if
!
    write (d_str,"(i8)"  ) d
    write (t_str,"(i4.4)") t
!
    tstr = d_str(1:4)//'/'//d_str(5:6)//'/'//d_str(7:)//' '//t_str(1:2)//':'//t_str(3:)//':00.000'
    call str2epoch ( tstr, epoch )
!
    if (present(timestring)) call epoch2str( epoch, timestring )
    if (present(error)) error=0_int32
!
    return
!
  End subroutine grib_get_epoch
!
!*****************************************************************************80


!*****************************************************************************80
!
! Reshape using pointers
!
!****************************
  Function grib_reshape_varSurf(array, shape_) result(aptr)
!****************************
!
   use iso_c_binding, only: C_LOC, C_F_POINTER
   implicit none
!
   ! Pass in the array as an array of fixed size so that there
   ! is no array descriptor associated with it. This means we
   ! can get a pointer to the location of the data using C_LOC
   real(single), dimension(:,:,:), intent(in), target :: array
   integer(int32), intent(in), dimension(4) :: shape_
   real(single), dimension(:,:,:,:), pointer :: aptr

   ! Use C_LOC to get the start location of the array data, and
   ! use C_F_POINTER to turn this into a fortran pointer (aptr).
   ! Note that we need to specify the shape of the pointer using an
   ! integer array.
   call C_F_POINTER(C_LOC(array), aptr, shape_)
!
  End function grib_reshape_varSurf
!
!*****************************************************************************80


!*****************************************************************************80
!
! Reshape using pointers
!
!****************************
  Function grib_reshape_varLevel(array, shape_) result(aptr)
!****************************
!
   use iso_c_binding, only: C_LOC, C_F_POINTER
   implicit none
!
   ! Pass in the array as an array of fixed size so that there
   ! is no array descriptor associated with it. This means we
   ! can get a pointer to the location of the data using C_LOC
   real(single), dimension(:,:,:,:), intent(in), target :: array
   integer(int32), intent(in), dimension(5) :: shape_
   real(single), dimension(:,:,:,:,:), pointer :: aptr

   ! Use C_LOC to get the start location of the array data, and
   ! use C_F_POINTER to turn this into a fortran pointer (aptr).
   ! Note that we need to specify the shape of the pointer using an
   ! integer array.
   call C_F_POINTER(C_LOC(array), aptr, shape_)
!
  End function grib_reshape_varLevel
!
!*****************************************************************************80
!
End module grib_types
