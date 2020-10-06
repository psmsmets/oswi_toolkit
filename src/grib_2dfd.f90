!*****************************************************************************80
!
!                             G R I B _ 2 D F D
!
!  Module:       grib_2dfd
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
!  Description:  This module includes several subroutines to read 2dfd ocean
!                wave spectra data and integrate over direction 
!                ( full direction range -> significant wave height, 
!                  opposite direction -> Hasselmann )
!
!
!*****************************************************************************80

Module grib_2dfd 

  use io_types
  use syms
  use time
  use eccodes
  use grib_types
  use iso_c_binding, only: C_LOC, C_F_POINTER
  use omp_lib

  implicit none

!
!..local grib types
!
  type type_grib_fspec
    logical                  :: defined, Hasselmann
    logical     , dimension(:)      , allocatable  :: mask
    real(double), dimension(:,:)    , allocatable  :: dat
    real(double), dimension(:,:,:)  , pointer      :: regular_ll, regular_ll_flip
  end type type_grib_fspec

  type type_grib_fdspec
    logical                  :: defined
    real(single), dimension(:,:,:)  , allocatable  :: dat
    real(single), dimension(:,:,:,:), pointer      :: regular_ll, regular_ll_flip
  end type type_grib_fdspec

  type type_grib_2dfd
    type(type_grib_grid)    :: grid
    type(type_grib_num)     :: number
    type(type_grib_step)    :: step
    type(type_grib_dir)     :: direction
    type(type_grib_frq)     :: frequency
    logical                 :: isECMWF, bitmapPresent
    integer(int32)          :: idx, paramId, experimentVersionNumber, editionNumber, &
                          oceanAtmosphereCoupling, missingValue
    integer(int64)          :: epoch
    character(len=time_len) :: time
    character(len=grib_alen)  :: shortName, longName, standard_name, units, centre, &
                          dataClass, dataType, dataStream, typeOfLevel
    type(type_grib_fspec)   :: fspec
    type(type_grib_fdspec)  :: fdspec
  end type type_grib_2dfd

Contains

! =========================================================================================
!
!   ..Subroutine grib_2dfd_clear
!
!       Initialize 2dfd structure.
!
!****************************
  Subroutine grib_2dfd_clear ( wam2dfd )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_2dfd), intent(inout), target  :: wam2dfd
!
!  ---
!
!   Common grib types
!
    call grib_clear_type_grid( wam2dfd%grid )
    call grib_clear_type_step( wam2dfd%step )
    call grib_clear_type_num( wam2dfd%number )
    call grib_clear_type_frq( wam2dfd%frequency )
    call grib_clear_type_dir( wam2dfd%direction )
!
!   General
!
    wam2dfd%isECMWF = .false.
    wam2dfd%idx = 0_int32
    wam2dfd%editionNumber = 0_int32
    wam2dfd%experimentVersionNumber = 0_int32
    wam2dfd%oceanAtmosphereCoupling = 0_int32
    wam2dfd%paramId = 0_int32
    wam2dfd%epoch = 0_int64
    wam2dfd%missingValue = 9999_int32
    wam2dfd%bitmapPresent = .false. 
    wam2dfd%time = ''
    wam2dfd%shortName = ''
    wam2dfd%longName = ''
    wam2dfd%standard_name = ''
    wam2dfd%units = ''
    wam2dfd%centre = ''
    wam2dfd%typeOfLevel = ''
    wam2dfd%dataClass = ''
    wam2dfd%dataType = ''
    wam2dfd%dataStream = ''
!
!   Data
!
    wam2dfd%fspec%defined = .false.
    wam2dfd%fspec%hasselmann = .true.
    if (allocated(wam2dfd%fspec%dat)) deallocate(wam2dfd%fspec%dat)
    if (associated(wam2dfd%fspec%regular_ll)) nullify(wam2dfd%fspec%regular_ll)
    if (associated(wam2dfd%fspec%regular_ll_flip)) nullify(wam2dfd%fspec%regular_ll_flip)
!
    wam2dfd%fdspec%defined = .false.
    if (allocated(wam2dfd%fdspec%dat)) deallocate(wam2dfd%fdspec%dat)
    if (associated(wam2dfd%fdspec%regular_ll)) nullify(wam2dfd%fdspec%regular_ll)
    if (associated(wam2dfd%fdspec%regular_ll_flip)) nullify(wam2dfd%fdspec%regular_ll_flip)
!
    return
!
  End subroutine grib_2dfd_clear
!
! =========================================================================================


! =========================================================================================
!
!   ..Subroutine grib_2dfd_list
!
!       List 2dfd variable information from grib file.
!
!****************************
  Subroutine grib_2dfd_list ( gribFile, wam2dfd, release_index, mask )
!****************************
!
    implicit none
!
!   Dummy variables
!
    character(len=*)    , intent(in)             :: gribFile
    type(type_grib_2dfd), intent(out), target    :: wam2dfd
    logical             , intent(in) , optional  :: release_index
    logical             , intent(out), optional  :: mask 
!
!   Local variables
!
    logical             :: exists, lrelease, stopOnError
    integer(int32)      :: idx, igrib, error, i, d, t, dummy
    character           :: dstr*8, tstr*4
!
!  ---
!
!   Initialize
!
    if (present(mask)) then 
      stopOnError=.false.
      mask=.false.
    else
      stopOnError=.true.
    end if
!
    lrelease = .true.
    if (present(release_index)) lrelease=release_index
!
    if (.not.allocated(wam2dfd%frequency%list).and..not.allocated(wam2dfd%direction%list).and. &
      & .not.allocated(wam2dfd%step%list).and..not.allocated(wam2dfd%number%list)) &
      & call grib_2dfd_clear(wam2dfd)
!
!   Open profile, if exists, stop otherwise.
!
    inquire( file=gribFile, exist=exists )
    if (.not.exists) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : grib file does not exist'
      call grib_2dfd_clear(wam2dfd)
      return
    end if
!
!   Open file and create index
!
    call codes_index_create ( idx, gribFile, 'shortName,number,step,direction,frequency', error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not create index for grib file'
      return
    end if
!   
!   Ensemble numbers
!
    call codes_index_get_size( idx, 'number', wam2dfd%number%size, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not get index size for ensemble number'
      return
    end if
    wam2dfd%number%count=wam2dfd%number%size
    if (allocated(wam2dfd%number%list).and.size(wam2dfd%number%list,1).ne.wam2dfd%number%size) &
      deallocate(wam2dfd%number%list)
    if (.not.allocated(wam2dfd%number%list)) allocate(wam2dfd%number%list(wam2dfd%number%size))
    call codes_index_get( idx, 'number', wam2dfd%number%list, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not get list of ensemble numbers'
      return
    end if
!
!   Forecast steps
!
    call codes_index_get_size( idx, 'step', wam2dfd%step%size, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not get index size for forecast step'
      return
    end if
    wam2dfd%step%count=wam2dfd%step%size
    if (allocated(wam2dfd%step%list).and.size(wam2dfd%step%list,1).ne.wam2dfd%step%size) &
      deallocate(wam2dfd%step%list)
    if (.not.allocated(wam2dfd%step%list)) allocate(wam2dfd%step%list(wam2dfd%step%size))
    call codes_index_get( idx, 'step', wam2dfd%step%list, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not get list of forecast steps'
      return
    end if
!   
!   get the number of distinct values of direction in the index
!
    call codes_index_get_size( idx, 'direction', wam2dfd%direction%size, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not get index size for direction'
      return
    end if
    wam2dfd%direction%count=>wam2dfd%direction%size
    if (allocated(wam2dfd%direction%list).and.size(wam2dfd%direction%list,1).ne.wam2dfd%direction%size) &
      deallocate(wam2dfd%direction%list)
    if (.not.allocated(wam2dfd%direction%list)) allocate(wam2dfd%direction%list(wam2dfd%direction%size))
    call codes_index_get( idx, 'direction', wam2dfd%direction%list, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not get list of directions'
      return
    end if
    if (allocated(wam2dfd%direction%dat).and.size(wam2dfd%direction%dat,1).ne.wam2dfd%direction%size) &
      deallocate(wam2dfd%direction%dat)
    if (.not.allocated(wam2dfd%direction%dat)) allocate(wam2dfd%direction%dat(wam2dfd%direction%size))
!  
!   get the number of distinct values of frequency in the index
!
    call codes_index_get_size( idx, 'frequency', wam2dfd%frequency%size, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not get index size for frequency'
      return
    end if
    if (allocated(wam2dfd%frequency%list).and.size(wam2dfd%frequency%list,1).ne.wam2dfd%frequency%size) &
      deallocate(wam2dfd%frequency%list)
    if (.not.allocated(wam2dfd%frequency%list)) allocate(wam2dfd%frequency%list(wam2dfd%frequency%size))
    call codes_index_get( idx, 'frequency', wam2dfd%frequency%list, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not get list of frequencies'
      return
    end if
    if (allocated(wam2dfd%frequency%dat).and.size(wam2dfd%frequency%dat,1).ne.wam2dfd%frequency%size) &
      deallocate(wam2dfd%frequency%dat)
    if (.not.allocated(wam2dfd%frequency%dat)) allocate(wam2dfd%frequency%dat(wam2dfd%frequency%size))
    if (allocated(wam2dfd%frequency%mask).and.size(wam2dfd%frequency%mask,1).ne.wam2dfd%frequency%size) &
      deallocate(wam2dfd%frequency%mask)
    if (.not.allocated(wam2dfd%frequency%mask)) allocate(wam2dfd%frequency%mask(wam2dfd%frequency%size))
    wam2dfd%frequency%mask=.true.
    wam2dfd%frequency%count=wam2dfd%frequency%size
!
!   Select first grid
!
    call codes_index_select( idx, 'shortName', '2dfd', error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not select index for shortName'
      return
    end if
    call codes_index_select( idx, 'number', wam2dfd%number%list(1), error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not select index 1 for ensemble number'
      return
    end if
    call codes_index_select( idx, 'step', wam2dfd%step%list(1), error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not select index 1 for forecast step'
      return
    end if
    call codes_index_select( idx, 'direction', wam2dfd%direction%list(1), error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not select index 1 for direction'
      return
    end if
    call codes_index_select( idx, 'frequency', wam2dfd%frequency%list(1), error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not select index 1 for frequency'
      return
    end if
!
!   get grib block from file opened by index
!
    call codes_new_from_index( idx, igrib, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not prepare data for selected index'
      return
    end if
!
!   Get date and time from grib file.
!
    call codes_get( igrib, 'dataDate', d, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not get dataDate'
      return
    end if
    call codes_get( igrib, 'dataTime', t, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not get dataTime'
      return
    end if
!
    write (dstr,"(i8)") d
    write (tstr,"(I4.4)") t
!
    wam2dfd%time = dstr(1:4)//'/'//dstr(5:6)//'/'//dstr(7:)//' '//tstr(1:2)//':'//tstr(3:)//':00.000'
    call str2epoch( wam2dfd%time, wam2dfd%epoch )
!
!   Get 2dfd variable info
!
    wam2dfd%shortName = '2dfd'
    wam2dfd%standard_name = 'sea_surface_wave_directional_variance_spectral_density'
!
    call codes_get( igrib, 'paramId', wam2dfd%paramId, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(paramId)'
      return
    end if
    call codes_get( igrib, 'name', wam2dfd%longName, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(name)'
      return
    end if
    call codes_get( igrib, 'units', wam2dfd%units, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(units)'
      return
    end if
    call codes_get( igrib, 'missingValue', wam2dfd%missingValue, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(missingValue)'
      return
    end if
    call codes_get( igrib, 'bitmapPresent', dummy, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(bitmapPresent)'
      return
    else
      wam2dfd%bitmapPresent=dummy.eq.1_int32
    end if
!
!   Get general info
!
    call codes_get( igrib, 'centre', wam2dfd%centre, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(centre)'
      return
    end if
    if (error.eq.0_int32) wam2dfd%isECMWF=wam2dfd%centre.eq.'ecmf'
    call codes_get( igrib, 'class', wam2dfd%dataClass, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(class)'
      return
    end if
    call codes_get( igrib, 'type', wam2dfd%dataType, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(type)'
      return
    end if
    call codes_get( igrib, 'stream', wam2dfd%dataStream, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(stream)'
      return
    end if
    call codes_get( igrib, 'editionNumber', wam2dfd%editionNumber, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(editionNumber)'
      return
    end if
    call codes_get( igrib, 'experimentVersionNumber', wam2dfd%experimentVersionNumber, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(experimentVersionNumber)'
      return
    end if
    call codes_get( igrib, 'oceanAtmosphereCoupling', wam2dfd%oceanAtmosphereCoupling, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(oceanAtmosphereCoupling)'
      return
    end if
    call codes_get( igrib, 'typeOfLevel', wam2dfd%typeOfLevel, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(typeOfLevel)'
      return
    end if
!
!   Get grid info
!
    call codes_get( igrib, 'gridType', wam2dfd%grid%gridType, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(gridType)'
      return
    end if
    call codes_get( igrib, 'numberOfPoints', wam2dfd%grid%numberOfPoints, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(numberOfPoints)'
      return
    end if
    call codes_get( igrib, 'gridName', wam2dfd%grid%gridName, error )
    call codes_get( igrib, 'packingType', wam2dfd%grid%packingType, error )
    call codes_get( igrib, 'isOctahedral', dummy, error )
    if (error.eq.0_int32) wam2dfd%grid%isOctahedral=dummy.eq.1_int32
    call codes_get( igrib, 'earthIsOblate', dummy, error )
    if (error.eq.0_int32) wam2dfd%grid%earthIsOblate=dummy.eq.64_int32
    call codes_get( igrib, 'shapeOfTheEarth', wam2dfd%grid%shapeOfTheEarth, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(shapeOfTheEarth)'
      return
    else
      select case (wam2dfd%grid%shapeOfTheEarth)
        case (0_int32)
!              Earth assumed spherical with radius = 6,367,470.0 m
          wam2dfd%grid%earthRadius=6367470_int32
!            case (1_int32)
!              Earth assumed spherical with radius specified (in m) by data producer
!            case (2_int32)
!              Earth assumed oblate spheroid with size as determined by IAU in 1965
!              (major axis = 6,378,160.0 m, minor axis = 6,356,775.0 m, f = 1/297.0)
!            case (3_int32)
!              Earth assumed oblate spheroid with major and minor axes specified (in km) by data producer
!            case (4_int32)
!              Earth assumed oblate spheroid as defined in IAG-GRS80 model
!              (major axis = 6,378,137.0 m, minor axis = 6,356,752.314 m, f = 1/298.257222101)
!            case (5_int32)
!              Earth assumed represented by WGS84 (as used by ICAO since 1998)
        case (6_int32)
!              Earth assumed spherical with radius of 6,371,229.0 m
          wam2dfd%grid%earthRadius=6371229_int32
!            case (7_int32)
!              Earth assumed oblate spheroid with major and minor axes specified (in m) by data producer
        case (8_int32)
!              Earth model assumed spherical with radius 6371200 m, but the horizontal datum of the 
!              resulting latitude/longitude field is the WGS84 reference frame
          wam2dfd%grid%earthRadius=6371200_int32
        case default
          stop 'Error @ grib_2dfd_list : shapeOfTheEarth should be spherical and of type 0, 6 or 8.'
      end select
    end if
!
!   Step parameters
!
    call codes_get( igrib, 'stepUnits', wam2dfd%step%units, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(stepUnits)'
      return
    end if
    call codes_get( igrib, 'stepRange', wam2dfd%step%stepRange, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(stepRange)'
      return
    end if
    call codes_get( igrib, 'startStep', wam2dfd%step%startStep, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(startStep)'
      return
    end if
    call codes_get( igrib, 'endStep', wam2dfd%step%endStep, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(endStep)'
      return
    end if
!
!   get latitude info
!
    call codes_get( igrib,'latitudeOfFirstGridPointInDegrees', wam2dfd%grid%lat%first, error ) 
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(latitudeOfFirstGridPointInDegrees)'
      return
    end if
    call codes_get( igrib, 'latitudeOfLastGridPointInDegrees', wam2dfd%grid%lat%last, error ) 
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(latitudeOfLastGridPointInDegrees)'
      return
    end if
    call codes_get( igrib, 'jDirectionIncrementInDegrees', wam2dfd%grid%lat%increment, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(jDirectionIncrementInDegrees)'
      return
    end if
    call codes_get_int( igrib, 'Ny', wam2dfd%grid%lat%size, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(Ny)'
      return
    end if
    wam2dfd%grid%lat%reversed = wam2dfd%grid%lat%first.gt.wam2dfd%grid%lat%last
!
!   get longitude first point
!
    call codes_get( igrib, 'longitudeOfFirstGridPointInDegrees',wam2dfd%grid%lon%first, error ) 
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(longitudeOfFirstGridPointInDegrees)'
      return
    end if
    call codes_get( igrib, 'longitudeOfLastGridPointInDegrees', wam2dfd%grid%lon%last, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(longitudeOfLastGridPointInDegrees)'
      return
    end if
    call codes_get( igrib, 'iDirectionIncrementInDegrees', wam2dfd%grid%lon%increment, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(iDirectionIncrementInDegrees)'
      return
    end if
    call codes_get_int( igrib, 'Nx', wam2dfd%grid%lon%size, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(Nx)'
      return
    end if
    wam2dfd%grid%lon%reversed = wam2dfd%grid%lon%first.gt.wam2dfd%grid%lon%last
!
!   Get latlon values
!
    wam2dfd%grid%distinctGridValues = wam2dfd%grid%gridType.eq.'regular_ll'
    if (wam2dfd%grid%distinctGridValues) then
      if (allocated(wam2dfd%grid%lat%dat).and.size(wam2dfd%grid%lat%dat,1).ne.wam2dfd%grid%lat%size) &
        deallocate(wam2dfd%grid%lat%dat)
      if (.not.allocated(wam2dfd%grid%lat%dat)) allocate(wam2dfd%grid%lat%dat(wam2dfd%grid%lat%size))
      call codes_get( igrib, 'distinctLatitudes' , wam2dfd%grid%lat%dat, error )
      if (error.ne.0_int32) then
        if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(distinctLatitudes)'
        return
      end if
      wam2dfd%grid%lat%ixFirst=1_int32
      wam2dfd%grid%lat%ixLast=wam2dfd%grid%lat%size
      wam2dfd%grid%lat%count=wam2dfd%grid%lat%size
!
      if (allocated(wam2dfd%grid%lon%dat).and.size(wam2dfd%grid%lon%dat,1).ne.wam2dfd%grid%lon%size) &
        deallocate(wam2dfd%grid%lon%dat)
      if (.not.allocated(wam2dfd%grid%lon%dat)) allocate(wam2dfd%grid%lon%dat(wam2dfd%grid%lon%size))
      call codes_get( igrib, 'distinctLongitudes', wam2dfd%grid%lon%dat, error ) 
      if (error.ne.0_int32) then
        if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(distinctLongitudes)'
        return
      end if
      wam2dfd%grid%lon%ixFirst=1_int32
      wam2dfd%grid%lon%ixLast=wam2dfd%grid%lon%size
      wam2dfd%grid%lon%count=wam2dfd%grid%lon%size
    else
      if (allocated(wam2dfd%grid%pl).and.size(wam2dfd%grid%pl,1).ne.wam2dfd%grid%lat%size) &
        deallocate(wam2dfd%grid%pl)
      if (.not.allocated(wam2dfd%grid%pl)) allocate(wam2dfd%grid%pl(wam2dfd%grid%lat%size))
      call codes_get( igrib, 'pl', wam2dfd%grid%pl, error ) 
      if (error.ne.0_int32) then
        if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(pl)'
        return
      end if
      if (allocated(wam2dfd%grid%lat%dat).and.size(wam2dfd%grid%lat%dat,1).ne.wam2dfd%grid%lat%size) &
        deallocate(wam2dfd%grid%lat%dat)
      if (.not.allocated(wam2dfd%grid%lat%dat)) allocate(wam2dfd%grid%lat%dat(wam2dfd%grid%lat%size))
      if (allocated(wam2dfd%grid%lat%mask).and.size(wam2dfd%grid%lat%mask,1).ne.wam2dfd%grid%lat%size) &
        deallocate(wam2dfd%grid%lat%mask)
      if (.not.allocated(wam2dfd%grid%lat%mask)) allocate(wam2dfd%grid%lat%mask(wam2dfd%grid%lat%size))
      wam2dfd%grid%lat%dat=(/ (i*wam2dfd%grid%lat%increment,i=0,wam2dfd%grid%lat%size-1) /)
      if (wam2dfd%grid%lat%reversed) then
        wam2dfd%grid%lat%dat=wam2dfd%grid%lat%first-wam2dfd%grid%lat%dat
      else
        wam2dfd%grid%lat%dat=wam2dfd%grid%lat%first+wam2dfd%grid%lat%dat
      end if
      wam2dfd%grid%lat%mask=wam2dfd%grid%pl.gt.0_int32
      wam2dfd%grid%lat%ixFirst=maxloc(wam2dfd%grid%lat%dat,dim=1,mask=wam2dfd%grid%lat%mask)
      wam2dfd%grid%lat%ixLast=minloc(wam2dfd%grid%lat%dat,dim=1,mask=wam2dfd%grid%lat%mask)
      wam2dfd%grid%lat%count=count(wam2dfd%grid%lat%mask)
      if (allocated(wam2dfd%grid%lon%dat).and.size(wam2dfd%grid%lon%dat,1).ne.wam2dfd%grid%numberOfPoints) &
        deallocate(wam2dfd%grid%lon%dat)
      if (.not.allocated(wam2dfd%grid%lon%dat)) allocate(wam2dfd%grid%lon%dat(wam2dfd%grid%numberOfPoints))
      call codes_get( igrib, 'longitudes', wam2dfd%grid%lon%dat, error ) 
      if (error.ne.0_int32) then
        if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(longitudes)'
        return
      end if
      wam2dfd%grid%lon%ixFirst=-1_int32
      wam2dfd%grid%lon%ixLast=-1_int32
      wam2dfd%grid%lon%count=wam2dfd%grid%lon%size
    end if
!
    wam2dfd%grid%lon%range=abs(wam2dfd%grid%lon%last-wam2dfd%grid%lon%first)
    wam2dfd%grid%lat%range=abs(wam2dfd%grid%lat%last-wam2dfd%grid%lat%first)
!
!   Direction
!
!      call codes_get( idx, 'numberOfDirections', wam2dfd%direction%size, error )
!      if (error.ne.0_int32) stop 'Error @ grib_2dfd_list : codes_get(numberOfDirections)'
    call codes_get( igrib, 'directionScalingFactor', wam2dfd%direction%scalingFactor, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(directionScalingFactor)'
      return
    end if
    call codes_get( igrib, 'scaledDirections', wam2dfd%direction%dat, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(scaledDirections)'
      return
    end if
    wam2dfd%direction%linearIncrement = all(wam2dfd%direction%list(2:wam2dfd%direction%size) &
      -wam2dfd%direction%list(1:wam2dfd%direction%size-1).eq.1_int32)
    wam2dfd%direction%dat=wam2dfd%direction%dat/wam2dfd%direction%scalingFactor
    wam2dfd%direction%increment=-1._double
    wam2dfd%direction%fullCircle=.false.
    if (wam2dfd%direction%linearIncrement) then
      wam2dfd%direction%increment=wam2dfd%direction%dat(2)-wam2dfd%direction%dat(1)
      wam2dfd%direction%fullCircle=nint(wam2dfd%direction%dat(wam2dfd%direction%size) &
        -wam2dfd%direction%dat(1)+wam2dfd%direction%increment,int32).eq.360_int32
    end if
!
!   Frequency
!
!      call codes_get( idx, 'numberOfFrequencies', wam2dfd%frequency%size, error )
!      if (error.ne.0_int32) stop 'Error @ grib_2dfd_list : codes_get(numberOfFrequencies)'
    call codes_get( igrib, 'frequencyScalingFactor', wam2dfd%frequency%scalingFactor, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(frequencyScalingFactor)'
      return
    end if
    call codes_get( igrib, 'scaledFrequencies', wam2dfd%frequency%dat, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : codes_get(scaledFrequencies)'
      return
    end if
    wam2dfd%frequency%dat=wam2dfd%frequency%dat/wam2dfd%frequency%scalingFactor
    wam2dfd%frequency%ixFirst=1_int32
    wam2dfd%frequency%ixLast=wam2dfd%frequency%size
!
!   release select grib message
!
    call codes_release( igrib, error )
    if (error.ne.0_int32) then
      if (stopOnError) stop 'Error @ grib_2dfd_list : could not release selected grib message'
      return
    end if
!
!   Close grib file
!
    if (lrelease) then
      call codes_index_release( idx, error )
      if (error.ne.0_int32) then
        if (stopOnError) then
          stop 'Error @ grib_2dfd_list : could not release grib index'
        else
          wam2dfd%idx = 0_int32
          return
        end if
      end if
    else
      wam2dfd%idx = idx
    end if
!
    if (present(mask)) mask=.true.
!
    return
!
  End subroutine grib_2dfd_list
!
! =========================================================================================


! =========================================================================================
!
!   ..Subroutine grib_2dfd_compare
!
!       Compare 2dfd structure.
!
!****************************
  Function grib_2dfd_compare ( wam2dfdA, wam2dfdB ) result ( match )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_2dfd), intent(in)  :: wam2dfdA, wam2dfdB
    logical                           :: match
!
!  ---
!
!   Grid
!
    match = grib_compare_type_grid( wam2dfdA%grid, wam2dfdB%grid )
    if (.not.match) return
!
!   Ensemble numbers
!
    match = grib_compare_type_num( wam2dfdA%number, wam2dfdB%number )
    if (.not.match) return
!
!   Forecast steps
!
    match = grib_compare_type_step( wam2dfdA%step, wam2dfdB%step )
    if (.not.match) return
!
!   Frequency
!
    match = grib_compare_type_frq( wam2dfdA%frequency, wam2dfdB%frequency )
    if (.not.match) return
!
!   Direction
!
    match = grib_compare_type_dir( wam2dfdA%direction, wam2dfdB%direction )
    if (.not.match) return
!
!   General
!
    match = wam2dfdA%paramId .eq. wam2dfdB%paramId
    if (.not.match) return
    match = wam2dfdA%shortName .eq. wam2dfdB%shortName
    if (.not.match) return
    match = wam2dfdA%longName .eq. wam2dfdB%longName
    if (.not.match) return
    match = wam2dfdA%units .eq. wam2dfdB%units
    if (.not.match) return
    match = wam2dfdA%centre .eq. wam2dfdB%centre
    if (.not.match) return
    match = wam2dfdA%isECMWF .eqv. wam2dfdB%isECMWF
    if (.not.match) return
!
    match = wam2dfdA%oceanAtmosphereCoupling.eq.wam2dfdB%oceanAtmosphereCoupling
    if (.not.match) return
    match = wam2dfdA%editionNumber.eq.wam2dfdB%editionNumber
    if (.not.match) return
    match = wam2dfdA%experimentVersionNumber.eq.wam2dfdB%experimentVersionNumber
    if (.not.match) return
!
    match = wam2dfdA%dataClass .eq. wam2dfdB%dataClass
    if (.not.match) return
    match = wam2dfdA%dataType .eq. wam2dfdB%dataType
    if (.not.match) return
    match = wam2dfdA%dataStream .eq. wam2dfdB%dataStream
    if (.not.match) return
    match = wam2dfdA%typeoflevel .eq. wam2dfdB%typeoflevel
    if (.not.match) return
!
    return
!
  End function grib_2dfd_compare
!
! =========================================================================================


! =========================================================================================
!
!   ..Function grib_2dfd_compare_gribFile_with_type
!
!       Compare 2dfd structure.
!
!****************************
  Function grib_2dfd_compare_gribFile_with_type ( gribFile, wam2dfd ) result ( match )
!****************************
!
    implicit none
!
!   Dummy variables
!
    character(len=*)    , intent(in)  :: gribFile
    type(type_grib_2dfd), intent(in)  :: wam2dfd
    logical                           :: match
!
!   Local variables
!
    type(type_grib_2dfd)  :: dummy
!
!  ---
!
    call grib_2dfd_list ( gribFile=trim(gribFile), wam2dfd=dummy, mask=match )
    if (.not.match) return
    match = grib_2dfd_compare( wam2dfd, dummy )
!
    return
!
  End function grib_2dfd_compare_gribFile_with_type
!
! =========================================================================================


! =========================================================================================
!
!   ..Subroutine grib_2dfd_set_frequency_range
!
!       Set 2dfd frequency range.
!
!****************************
  Subroutine grib_2dfd_set_frequency_range ( frequency, indexOfFirstFrequency, indexOfLastFrequency, &
             firstFrequency, lastFrequency, mask, doubled )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_frq), intent(inout), target    :: frequency
    integer(int32)     , intent(in)   , optional  :: indexOfFirstFrequency, indexOfLastFrequency
    real(double)       , intent(in)   , optional  :: firstFrequency, lastFrequency
    logical, dimension(frequency%count), intent(in), optional  :: mask
    logical             , intent(in)   , optional  :: doubled
!
!   Local variables
!
    logical                 :: doubled_
    integer(int32), pointer :: ixFirst, ixLast
    real(double)            :: frqFirst, frqLast
!
!  ---
!
!
    doubled_ = .false.
    if (present(doubled)) doubled_=doubled
!
    ixFirst => frequency%ixFirst
    ixLast  => frequency%ixLast
!
!   Different io options
!
    if (present(mask)) then
      frequency%mask=mask
      ixFirst=-1_int32
      ixLast=-1_int32
    else if( present(indexOfFirstFrequency).and.present(indexOfLastFrequency) ) then
      if(indexOfFirstFrequency.lt.1_int32.or.indexOfFirstFrequency.gt.frequency%size.or.&
        indexOfLastFrequency .lt.1_int32.or.indexOfLastFrequency .gt.frequency%size) &
        stop 'Error @ grib_2dfd_set_frequency_range : illegal indexes given!'
      frequency%mask=.false.
      frequency%mask(indexOfFirstFrequency:indexOfLastFrequency)=.true.
      ixFirst=indexOfFirstFrequency
      ixLast=indexOfLastFrequency
    else if( present(firstFrequency).and.present(lastFrequency) ) then
      frqFirst=firstFrequency
      if (abs(firstFrequency+1._double).lt.1e-6) then
        ixFirst=1_int32
      else
        if (doubled_) frqFirst=frqFirst/2
        ixFirst=minloc(abs(frequency%dat-frqFirst),1,frequency%dat-frqFirst.lt.0._double)
        if (ixFirst.lt.1_int32) ixFirst=1_int32
        if (ixFirst.gt.frequency%size) ixFirst=frequency%size
      end if
      frqLast=lastFrequency
      if (abs(lastFrequency+1._double).lt.1e-6) then
        ixLast=frequency%size
      else
        if (doubled_) frqLast=frqLast/2
        ixLast=minloc(abs(frqLast-frequency%dat),1,frqLast-frequency%dat.gt.0._double)+1_int32
        if (ixLast.gt.frequency%size) ixLast=frequency%size
        if (ixLast.lt.1_int32) ixLast=1_int32
      end if
      frequency%mask=.false.
      frequency%mask(ixFirst:ixLast)=.true.
    else
      stop 'Error @ grib_2dfd_set_frequency_range : either mask vector, indexes or values expected!'
    end if
!
    frequency%count=count(frequency%mask)
!
    return
!
  End subroutine grib_2dfd_set_frequency_range
!
! =========================================================================================


! =========================================================================================
!
!   ..subroutine grib_2dfd_print
!
!       print 2dfd from structure.
!
!****************************
  subroutine grib_2dfd_print ( wam2dfd )
!****************************
!
    implicit none
!
!   Dummy variables
!
    type(type_grib_2dfd), intent(in)    :: wam2dfd
!
!   Local variables
!
    integer(int32) :: i
!
!  ---
!
    write (output_unit,"(3a)") '# // ', trim(wam2dfd%longName), ' //'
    write (output_unit,"(3a,i6,a)") '# shortname (id)    = ', &
       trim(wam2dfd%shortName), ' (', wam2dfd%paramId, ')'
    write (output_unit,"(2a)")      '# units             = ', trim(wam2dfd%units)
    write (output_unit,"(2a)")      '# date_time         = ', wam2dfd%time
    write (output_unit,"(2a)")      '# centre            = ', wam2dfd%centre
    write (output_unit,"(2a)")      '# class             = ', wam2dfd%dataClass
    write (output_unit,"(2a)")      '# type              = ', wam2dfd%dataType
    write (output_unit,"(2a)")      '# stream            = ', wam2dfd%dataStream
    write (output_unit,"(a,i10)")   '# editionNumber     = ', wam2dfd%editionNumber
    write (output_unit,"(a,i10)")   '# ExpversionNumber  = ', wam2dfd%experimentVersionNumber
    write (output_unit,"(2a)")      '# type_of_level     = ', wam2dfd%typeoflevel
!
    if (wam2dfd%step%count.gt.1_int32) then
      write (output_unit,"(a,i4)") '# forecast steps    = ', wam2dfd%step%count
      write (output_unit,"(a)",advance='no') '# step list         ='
      do i = 1, wam2dfd%step%count
        write (output_unit,"(1x,i4)",advance='no') wam2dfd%step%list( i )
      end do
      write (output_unit,"(a)") ''
    end if
    if (wam2dfd%number%count.gt.1_int32) then
      write (output_unit,"(a,i4)") '# ensemble numbers  = ', wam2dfd%number%count
      write (output_unit,"(a)",advance='no') '# ensemble list     ='
      do i = 1, wam2dfd%number%count
        write (output_unit,"(1x,i4)",advance='no') wam2dfd%number%list( i )
      end do
      write (output_unit,"(a)") ''
    end if
!
    if (len_trim(wam2dfd%grid%gridName).gt.0_int32) &
      write (output_unit,"(2a)")  '# grid_gridName     = ', wam2dfd%grid%gridName
    if (len_trim(wam2dfd%grid%packingType).gt.0_int32) &
      write (output_unit,"(2a)")  '# grid_packingType  = ', wam2dfd%grid%packingType
    write (output_unit,"(2a)")     '# grid_gridType     = ', wam2dfd%grid%gridType
    write (output_unit,"(a,i8)")   '# grid_nofpoints    = ', wam2dfd%grid%numberOfPoints
!
    if (allocated(wam2dfd%grid%lat%mask)) then
      write (output_unit,"(a,f8.3,' (',f8.3,')')") '# lat_first         = ', wam2dfd%grid%lat%first, &
        wam2dfd%grid%lat%dat(wam2dfd%grid%lat%ixFirst)
      write (output_unit,"(a,f8.3,' (',f8.3,')')") '# lat_last          = ', wam2dfd%grid%lat%last, &
        wam2dfd%grid%lat%dat(wam2dfd%grid%lat%ixLast)
      write (output_unit,"(a,f8.3)") '# lat_increment     = ', wam2dfd%grid%lat%increment
      write (output_unit,"(a,i8,' (',i8,')')"    ) '# lat_N             = ', wam2dfd%grid%lat%size, &
        wam2dfd%grid%lat%count
    else
      write (output_unit,"(a,f8.3)") '# lat_first         = ', wam2dfd%grid%lat%first
      write (output_unit,"(a,f8.3)") '# lat_last          = ', wam2dfd%grid%lat%last
      write (output_unit,"(a,f8.3)") '# lat_increment     = ', wam2dfd%grid%lat%increment
      write (output_unit,"(a,i8)"  ) '# lat_N             = ', wam2dfd%grid%lat%size
    end if
!
    if (allocated(wam2dfd%grid%lon%mask)) then
      write (output_unit,"(a,f8.3,' (',f8.3,')')") '# lon_first         = ', wam2dfd%grid%lon%first, &
        wam2dfd%grid%lon%dat(wam2dfd%grid%lon%ixFirst)
      write (output_unit,"(a,f8.3,' (',f8.3,')')") '# lon_last          = ', wam2dfd%grid%lon%last, &
        wam2dfd%grid%lon%dat(wam2dfd%grid%lon%ixLast)
      if (wam2dfd%grid%distinctGridValues) &
        write (output_unit,"(a,f8.3)") '# lon_increment     = ', wam2dfd%grid%lon%increment
      write (output_unit,"(a,i8,' (',i8,')')"    ) '# lon_N             = ', wam2dfd%grid%lon%size, &
        wam2dfd%grid%lon%count
    else
      write (output_unit,"(a,f8.3)") '# lon_first         = ', wam2dfd%grid%lon%first
      write (output_unit,"(a,f8.3)") '# lon_last          = ', wam2dfd%grid%lon%last
      if (wam2dfd%grid%distinctGridValues) &
        write (output_unit,"(a,f8.3)") '# lon_increment     = ', wam2dfd%grid%lon%increment
      write (output_unit,"(a,i8)"  ) '# lon_N             = ', wam2dfd%grid%lon%count
    end if
!
    write (output_unit,"(a,f8.3)")   '# direction_first   = ', wam2dfd%direction%dat(1)
    write (output_unit,"(a,f8.3)")   '# direction_last    = ', wam2dfd%direction%dat(wam2dfd%direction%size)
    write (output_unit,"(a,i2)")     '# direction_N       = ', wam2dfd%direction%size
    write (output_unit,"(a,f8.3)")   '# direction_incr    = ', wam2dfd%direction%increment
    write (output_unit,"(a,l1)"  )   '# direction_2pi     = ', wam2dfd%direction%fullCircle
    write (output_unit,"(a,f9.4)")   '# frequency_first   = ', wam2dfd%frequency%dat(1)
    write (output_unit,"(a,f9.4)")   '# frequency_last    = ', wam2dfd%frequency%dat(wam2dfd%frequency%size)
    write (output_unit,"(2(a,i2),a)")'# frequency_N       = ', &
       wam2dfd%frequency%size, ' (',wam2dfd%frequency%count,')'
!
    return
!
  End subroutine grib_2dfd_print
!
! =========================================================================================


! =========================================================================================
!
!   ..Subroutine grib_2dfd_get_integrated_spectrum_data
!
!       Subroutine to read 2DFD data from ECMWF WAM model (GRIB file). To reduce memory the
!       Hasselmann integral is calculated directly, reducing the amount of data to only
!       frequency depending.
!
!****************************
  Subroutine grib_2dfd_get_integrated_spectrum_data ( gribFile, wam2dfd, fspec, Hasselmann, &
    forecastStep, ensembleNumber, mask, verbose, experimentVersionNumber, epoch )
!****************************
!
    implicit none
!
!   Dummy variables
!
    character(len=*)    , intent(in   ), optional                 :: gribFile
    type(type_grib_2dfd), intent(inout), target                   :: wam2dfd
    real(double), dimension(wam2dfd%grid%numberOfPoints,wam2dfd%frequency%count), &
                          intent(out  ), target                   :: fspec
    logical             , intent(in   ), optional                 :: Hasselmann 
    integer(int32)      , intent(in   ), optional                 :: forecastStep, ensembleNumber
    logical, dimension(wam2dfd%grid%numberOfPoints,wam2dfd%frequency%count), &
                          intent(out  ), optional                 :: mask
    logical             , intent(in   ), optional                 :: verbose
    integer(int32)      , intent(out)  , optional                 :: experimentVersionNumber
    integer(int64)      , intent(out)  , optional                 :: epoch
!
!   Local variables
!
    logical         :: exists, hasselmann_, verb, getDetails
    integer(int32)  :: cFrq, ixnumber, ixstep
    integer(int32)  :: error, idx, igrib, idir, ifrq, nofdir2 
    real(double)    :: twopi__nofdir, fourpi__nofdir
!
    logical       , dimension(wam2dfd%grid%numberOfPoints,wam2dfd%direction%size), target  :: msk
    integer(int32), dimension(wam2dfd%grid%numberOfPoints), target  :: bitmap!, bitcount
    real(double)  , dimension(wam2dfd%grid%numberOfPoints,wam2dfd%direction%size), target  :: tmp
!
    logical     , dimension(:)  , pointer  :: msk1
    logical     , dimension(:,:), pointer  :: mskdir1, mskdir2
    integer(int32)              , pointer  :: nofdir
    real(double), dimension(:)  , pointer  :: values
    real(double), dimension(:,:), pointer  :: dir1, dir2
!
!  ---
!
!   initialize
!
    verb=.false.
    if (present(verbose)) verb=verbose
    if (verb) write (output_unit,"(a)") '> grib_2dfd_get_integrated_spectrum_data'
!
    ixstep = 1_int32
    if (present(forecastStep)) then
      ixstep=forecastStep
    else
      ixstep=wam2dfd%step%sel
    end if
    if (ixstep.lt.1_int32.or.ixstep.gt.wam2dfd%step%count) ixstep=1_int32
    wam2dfd%step%sel=ixstep
!
    ixnumber = 1_int32
    if (present(ensembleNumber)) then
      ixnumber=ensembleNumber
    else
      ixnumber=wam2dfd%number%sel
    end if
    if (ixnumber.lt.1_int32.or.ixnumber.gt.wam2dfd%number%count) ixnumber=1_int32
    wam2dfd%number%sel=ixnumber
!
    hasselmann_=wam2dfd%fspec%hasselmann
    if (present(hasselmann)) hasselmann_=hasselmann
    if (verb) write (output_unit,"(4x,a,l1)") 'Hasselmann = ', hasselmann_
!
    getDetails     = .true.
    fspec          = 0._double
    cFrq           = 0_int32
    nofdir         => wam2dfd%direction%size
    nofdir2        = nofdir/2
    twopi__nofdir  = twopi/nofdir
    fourpi__nofdir = 2*twopi/nofdir
!
!   Check directional range
!
    if (.not.wam2dfd%direction%fullCircle) write (output_unit,"(a)") 'Warning: 2dfd directional spectra is no full circle!'
!
!   Index already created?
!
    if (wam2dfd%idx.eq.0_int32.and..not.present(gribFile)) then
      stop 'Error @ grib_2dfd_get_directional_spectrum : No grib file provided and no grib index created yet'
    elseif (wam2dfd%idx.eq.0_int32.and.present(gribFile)) then
      inquire( file = gribFile, exist = exists )
      if (.not.exists) stop 'Error @ grib_2dfd_get_integrated_spectrum : Grib file does not exist'
      call codes_index_create ( idx, gribFile, 'shortName,number,step,directionNumber,frequencyNumber', error )
      if (error.ne.0_int32) stop 'Error @ grib_2dfd_get_integrated_spectrum Could not open file'
    else
      idx=wam2dfd%idx
    end if
!
!   set variable to 2dfd
!
    call codes_index_select( idx, 'shortName', '2dfd' )
    if (error.ne.0_int32) stop 'Error @ grib_2dfd_get_integrated_spectrum : could not select index for shortName'
!
!   set forecast step and ensemble number 
!
    call codes_index_select( idx, 'step', wam2dfd%step%list(ixstep) )
    call codes_index_select( idx, 'number', wam2dfd%number%list(ixnumber) )
!
!   loop on frequency
!
    do ifrq=1_int32, wam2dfd%frequency%size
!
      if (.not.wam2dfd%frequency%mask(ifrq)) cycle
      cFrq=count(wam2dfd%frequency%mask(1_int32:ifrq))
!      bitcount=0_int32
      msk=.false.
      tmp=0._double
      call codes_index_select( idx, 'frequencyNumber', wam2dfd%frequency%list(cFrq) )
      if (verb) write (*,"(4x,'>> ',a,i2,1x,f6.4,'Hz ')",advance='no') 'read directions for frequency ', &
        wam2dfd%frequency%list(cFrq), wam2dfd%frequency%dat(cFrq)
!
!     loop on direction
!
      do idir=1_int32, nofdir
!
        if (verb) write (*,"(a)",advance='no') '.'
        call codes_index_select( idx, 'directionNumber', wam2dfd%direction%list(idir) )
        call codes_new_from_index ( idx, igrib )
!
!       Get additional parameters
!
        if (getDetails) then
          call codes_get( igrib, 'experimentVersionNumber', wam2dfd%experimentVersionNumber, error )
          if (present(experimentVersionNumber)) experimentVersionNumber=wam2dfd%experimentVersionNumber
          call grib_get_epoch( igrib=igrib, epoch=wam2dfd%epoch, error=error )
          if (present(epoch)) epoch=wam2dfd%epoch
          getDetails=.false.
        end if
!
        values => tmp(:,idir)
        msk1   => msk(:,idir)
        call grib_get( igrib, 'values', values )
!
!       Convert log10 values
!
        if (wam2dfd%bitmapPresent) then
          call grib_get( igrib, 'bitmap', bitmap )
          msk1=bitmap.eq.1
        else
          msk1=int(values).ne.wam2dfd%missingValue
          bitmap=transfer(msk1,(/1,0/))
        end if
!         bitcount=bitcount+bitmap
         values=bitmap*(10._double**(bitmap*values))
!
        call codes_release( igrib )
!
      end do
!
!     Output depends on data type requested: hasselmann integral or significant wave spectrum!
!
      values=>fspec(:,cFrq)
!
      if (verb) write (output_unit,"(a)") ' integrate.'
      if ( Hasselmann_ ) then
!
!     Perform Hasselmann integral. Change units (f -> omega), multiply with (2pi) ** 2
!
!     integrate from 0 to 2*pi, but Hasselmann integral is only 0 to 1*pi, so integrate from 0 to pi
!     and multiply with 2. No boundary issues due to integration along a circle using 
!     Trapezoidal rule: h = 2pi/(2N)
!
!     Multiply all by 2 as Hasselmann (dir1*dir2) is only defined from 0 to pi
!
!     Total scalar becomes: (2pi)**2 * h * 2 = (2pi)**3 / N
!
        dir1 => tmp(:,1_int32:nofdir2)
        dir2 => tmp(:,nofdir2+1_int32:nofdir)
        dir1 = dir1 * dir2
        values = fourpi__nofdir * sum(dir1,dim=2)
!
        if (present(mask)) then
          mskdir1 => msk(:,1_int32:nofdir2)
          mskdir2 => msk(:,nofdir2+1_int32:nofdir)
          mask(:,cFrq)=any(mskdir1.and.mskdir2,dim=2)
        end if
!
      else
!
!       integrate over direction using trapezoidal rule along a circle: h = 2pi/2N
!
        values = twopi__nofdir * sum(tmp,dim=2)
        if (present(mask)) mask(:,cFrq)=any(msk,dim=2)
!
      end if
!
    end do
!
!   release grib file index
!
    call codes_index_release( idx )
!
    nullify(values)
    if(Hasselmann_) nullify(dir1,dir2)
    if (present(mask).and.Hasselmann_) nullify(mskdir1,mskdir2)
!
    return
!
  End subroutine grib_2dfd_get_integrated_spectrum_data 
!
! =========================================================================================


! =========================================================================================
!
!   ..Subroutine grib_2dfd_get_integrated_spectrum
!
!       Subroutine to read 2DFD data from ECMWF WAM model (GRIB file). To reduce memory the
!       Hasselmann integral is calculated directly, reducing the amount of data to only
!       frequency depending.
!
!****************************
  Subroutine grib_2dfd_get_integrated_spectrum ( gribFile, wam2dfd, Hasselmann, &
    forecastStep, ensembleNumber )
!****************************
!
    implicit none
!
!   Dummy variables
!
    character(len=*)    , intent(in   ), optional  :: gribFile
    type(type_grib_2dfd), intent(inout), target    :: wam2dfd
    logical             , intent(in   ), optional  :: Hasselmann 
    integer(int32)      , intent(in   ), optional  :: forecastStep, ensembleNumber
!
!  ---
!
!   initialize
!
    if (allocated(wam2dfd%fspec%dat).and.(size(wam2dfd%fspec%dat,1).ne.wam2dfd%grid%numberOfPoints&
      .or.size(wam2dfd%fspec%dat,2).ne.wam2dfd%frequency%count)) deallocate(wam2dfd%fspec%dat)
    if (.not.allocated(wam2dfd%fspec%dat)) &
      allocate(wam2dfd%fspec%dat(wam2dfd%grid%numberOfPoints,wam2dfd%frequency%count))
!
    if (allocated(wam2dfd%fspec%mask).and.size(wam2dfd%fspec%mask).ne.wam2dfd%grid%numberOfPoints) &
      deallocate(wam2dfd%fspec%mask)
    if (.not.allocated(wam2dfd%fspec%mask)) allocate(wam2dfd%fspec%mask(wam2dfd%grid%numberOfPoints))
!
!....Get integrated spectra + mask
!
    call grib_2dfd_get_integrated_spectrum_data ( &
      gribFile       = gribFile, &
      wam2dfd        = wam2dfd, &
      fspec          = wam2dfd%fspec%dat, &
      Hasselmann     = Hasselmann, &
      forecastStep   = forecastStep, &
      ensembleNumber = ensembleNumber &
    )
!
!   Regular grid?
!
    if (wam2dfd%grid%distinctGridValues) then
      wam2dfd%fspec%regular_ll => grib_2dfd_reshape_fspec( wam2dfd%fspec%dat, &
        (/wam2dfd%grid%lon%count,wam2dfd%grid%lat%count,wam2dfd%frequency%count/) )
      wam2dfd%fspec%regular_ll_flip => wam2dfd%fspec%regular_ll(:,wam2dfd%grid%lat%count:1_int32:-1_int32,:)
    end if
    wam2dfd%fspec%defined=.true.
!
    return
!
  End subroutine grib_2dfd_get_integrated_spectrum 
!
! =========================================================================================


! =========================================================================================
!
!   ..subroutine grib_2dfd_get_directional_spectrum_data
!
!       read ECMWF wam 2dfd data from grib file
!
!****************************
  Subroutine grib_2dfd_get_directional_spectrum_data ( gribFile, wam2dfd, fdspec, &
    forecastStep, ensembleNumber, experimentVersionNumber, epoch )
!****************************
!
    implicit none
!
!   Dummy variables
!
    character(len=*)    , intent(in)   , optional  :: gribFile
    type(type_grib_2dfd), intent(inout), target    :: wam2dfd
    real(single), dimension(wam2dfd%grid%numberOfPoints,wam2dfd%frequency%count,&
    wam2dfd%direction%count), intent(out), target  :: fdspec
    integer(int32)      , intent(in)   , optional  :: forecastStep, ensembleNumber
    integer(int32)      , intent(out)  , optional  :: experimentVersionNumber
    integer(int64)      , intent(out)  , optional                 :: epoch
!
!   local variables
!
    logical            :: exists, getDetails
    integer(int32)     :: ixstep, ixnumber, idir, ifrq
    integer(int32)     :: error, idx, igrib
!
    real(single), dimension(:), pointer  :: values
!
!  ---
!
!   initialize
!
    ixstep = 1_int32
    if (present(forecastStep)) then
      if (forecastStep.gt.0_int32.and.forecastStep.le.wam2dfd%step%count) ixstep=forecastStep
    end if
    wam2dfd%step%sel=int(ixstep,kind=int32)
!
    ixnumber = 1_int32
    if (present(ensembleNumber)) then
      if (ensembleNumber.gt.0_int32.and.ensembleNumber.le.wam2dfd%number%count) ixnumber=ensembleNumber
    end if
    wam2dfd%number%sel=int(ixnumber,kind=int32)
!
    getDetails=.true.
!
!   Check directional range
!
    if (.not.wam2dfd%direction%fullCircle) write (output_unit,"(a)") &
      'Warning: 2dfd directional spectra is no full circle!'
!
!   Index already created?
!
    if (wam2dfd%idx.eq.0_int32.and..not.present(gribFile)) then
      write (error_unit,"(2a)") 'Error @ grib_2dfd_get_directional_spectrum : ', &
        'No grib file provided and no grib index created yet'
      stop
    elseif (wam2dfd%idx.eq.0_int32.and.present(gribFile)) then
      inquire( file = gribFile, exist = exists )
      if (.not.exists) then
        write (error_unit,"(2a)") 'Error @ grib_2dfd_get_directional_spectrum : ', &
          'Grib file does not exist'
        stop
      end if
      call codes_index_create ( idx, gribFile, 'shortName,number,step,directionNumber,frequencyNumber', &
         error )
      if (error.ne.0_int32) then
        write (error_unit,"(a)") 'Error @ grib_2dfd_get_directional_spectrum : Could not open file'
        stop
      endif
    else
      idx=wam2dfd%idx
    end if
!
!   set variable to 2dfd
!
    call codes_index_select( idx, 'shortName', '2dfd' )
!
!   set forecast step and ensemble number 
!
    call codes_index_select( idx, 'step', wam2dfd%step%list(ixstep), error )
    call codes_index_select( idx, 'number', wam2dfd%number%list(ixnumber), error )
!
!   loop on frequency
!
    do ifrq=1_int32, wam2dfd%frequency%count
!
      call codes_index_select( idx, 'frequencyNumber', wam2dfd%frequency%list(ifrq) )
!
!   ...loop on direction
!
      do idir=1_int32, wam2dfd%direction%count
!
        call codes_index_select( idx, 'directionNumber', wam2dfd%direction%list(idir) )
!
        call codes_new_from_index( idx, igrib, error )
        if (error.ne.0_int32) cycle
!
!       Get additional parameters
!
        if (getDetails) then
          call codes_get( igrib, 'experimentVersionNumber', wam2dfd%experimentVersionNumber, error )
          if (present(experimentVersionNumber)) experimentVersionNumber=wam2dfd%experimentVersionNumber
          call grib_get_epoch( igrib=igrib, epoch=wam2dfd%epoch, error=error )
          if (present(epoch)) epoch=wam2dfd%epoch
          getDetails=.false.
        end if
!
        values => fdspec(:,ifrq,idir)
        call grib_get( igrib, 'values', values )
        call codes_release( igrib, error )
!
      end do
!
    end do
!
!   get experiment version number
!
    call codes_get( igrib, 'experimentVersionNumber', wam2dfd%experimentVersionNumber, error )
    if (present(experimentVersionNumber)) experimentVersionNumber=wam2dfd%experimentVersionNumber
!
!   release grib file index
!
    call codes_index_release( idx )
    nullify(values)
!
    return
!
  End subroutine grib_2dfd_get_directional_spectrum_data
!
! =========================================================================================


! =========================================================================================
!
!   ..subroutine grib_2dfd_get_directional_spectrum
!
!       read ECMWF wam 2dfd data from grib file
!
!****************************
  Subroutine grib_2dfd_get_directional_spectrum ( gribFile, wam2dfd, forecastStep, ensembleNumber )
!****************************
!
    implicit none
!
!   Dummy variables
!
    character(len=*)    , intent(in)   , optional  :: gribFile
    type(type_grib_2dfd), intent(inout), target    :: wam2dfd
    integer(int32)      , intent(in)   , optional  :: forecastStep, ensembleNumber
!
!  ---
!
!   initialize
!
    if (.not.allocated(wam2dfd%fdspec%dat)) then
      allocate(wam2dfd%fdspec%dat(wam2dfd%grid%numberOfPoints,wam2dfd%frequency%count,wam2dfd%direction%count))
    else
      if (size(wam2dfd%fdspec%dat,1).ne.wam2dfd%grid%numberOfPoints.or.  &
         size(wam2dfd%fdspec%dat,2).ne.wam2dfd%frequency%count.or. &
         size(wam2dfd%fdspec%dat,3).ne.wam2dfd%direction%count     &
        ) then
        deallocate(wam2dfd%fdspec%dat)
        allocate(wam2dfd%fdspec%dat(wam2dfd%grid%numberOfPoints,wam2dfd%frequency%count,wam2dfd%direction%count))
      end if
    end if
!
    call grib_2dfd_get_directional_spectrum_data ( &
      gribFile       = gribFile, &
      wam2dfd        = wam2dfd, &
      fdspec         = wam2dfd%fdspec%dat, &
      forecastStep   = forecastStep, &
      ensembleNumber = ensembleNumber &
    )
!
!   Regular grid?
!
    if (wam2dfd%grid%distinctGridValues) then
      wam2dfd%fdspec%regular_ll => grib_2dfd_reshape_fdspec( wam2dfd%fdspec%dat, &
        (/wam2dfd%grid%lon%count,wam2dfd%grid%lat%count,wam2dfd%frequency%count,wam2dfd%direction%count/) )
      wam2dfd%fdspec%regular_ll_flip => wam2dfd%fdspec%regular_ll(:,wam2dfd%grid%lat%count:1_int32:-1_int32,:,:)
    end if
    wam2dfd%fdspec%defined=.true.
!
    return
!
  End subroutine grib_2dfd_get_directional_spectrum
!
! =========================================================================================


! =========================================================================================
!
!   ..Reshape using pointers
!
!****************************
  Function grib_2dfd_reshape_ll(array, shape_) result(aptr)
!****************************
!
   use iso_c_binding, only: C_LOC, C_F_POINTER
   implicit none
!
   ! Pass in the array as an array of fixed size so that there
   ! is no array descriptor associated with it. This means we
   ! can get a pointer to the location of the data using C_LOC
   real(double), dimension(:), intent(in), target :: array
   integer(int32), intent(in), dimension(2) :: shape_
   real(double), dimension(:,:), pointer :: aptr

   ! Use C_LOC to get the start location of the array data, and
   ! use C_F_POINTER to turn this into a fortran pointer (aptr).
   ! Note that we need to specify the shape of the pointer using an
   ! integer array.
   call C_F_POINTER(C_LOC(array), aptr, shape_)
!
  End function grib_2dfd_reshape_ll
!
! =========================================================================================


! =========================================================================================
!
!   ..Reshape using pointers
!
!****************************
  Function grib_2dfd_reshape_fspec(array, shape_) result(aptr)
!****************************
!
   use iso_c_binding, only: C_LOC, C_F_POINTER
   implicit none
!
   ! Pass in the array as an array of fixed size so that there
   ! is no array descriptor associated with it. This means we
   ! can get a pointer to the location of the data using C_LOC
   real(double), dimension(:,:), intent(in), target :: array
   integer(int32), intent(in), dimension(3) :: shape_
   real(double), dimension(:,:,:), pointer :: aptr

   ! Use C_LOC to get the start location of the array data, and
   ! use C_F_POINTER to turn this into a fortran pointer (aptr).
   ! Note that we need to specify the shape of the pointer using an
   ! integer array.
   call C_F_POINTER(C_LOC(array), aptr, shape_)
!
  End function grib_2dfd_reshape_fspec
!
! =========================================================================================


! =========================================================================================
!
!   ..Reshape using pointers
!
!****************************
  Function grib_2dfd_reshape_fdspec(array, shape_) result(aptr)
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
  End function grib_2dfd_reshape_fdspec
!
! =========================================================================================


! =========================================================================================
!
!   ..Subroutine grib_2dfd_reduced_to_regular_ll
!
!       Make regular_ll grid of 2dfd data using bilinear interpolation.
!
!****************************
  Subroutine grib_2dfd_reduced_to_regular_ll ( grid, reduced_ll, regular_ll, missingValue )
!****************************
!
    use math, only: distance
    implicit none
!
!   Dummy variables
!
    type(type_grib_grid)      , intent(in) , target       :: grid 
    real(double), dimension(:), intent(in) , target       :: reduced_ll
    real(double), dimension(:), intent(out), allocatable  :: regular_ll
    integer(int32)            , intent(in) , optional     :: missingValue
!
!   Local variables
!
    logical                  :: fixMissing
    integer(int32)           :: nlon, i, ilon, cnt, pnt, a, b, npoints
    integer(int32), pointer  :: pl
    real(double)             :: dax, dab, dpl
    real(double), pointer    :: reducedLat
!
    real(double), dimension(nint((grid%lon%last-grid%lon%first)/&
      grid%lat%increment+1_int32,int32)), target  :: regularLon
    real(double), dimension(:), pointer      :: reducedLon
    real(double), dimension(:), pointer      :: reducedDat
!
!  ---
!
!   Check
!
    if (grid%gridType.ne.'reduced_ll') &
      stop 'Error @ grib_2dfd_reduced_to_regular_ll : reduced_ll input grid expected!'
!
    fixMissing = present(missingValue)
!
!   New grid
!
    nlon = nint((grid%lon%last-grid%lon%first)/grid%lat%increment+1_int32,int32)
    regularLon = (/ (grid%lon%first+i*grid%lat%increment,i=0_int32,nlon-1_int32,1_int32) /)
    npoints = nlon*grid%lat%count
!
    if (allocated(regular_ll).and.size(regular_ll,1).ne.npoints) &
      deallocate(regular_ll)
    if (.not.allocated(regular_ll)) allocate(regular_ll(npoints))
!
    if (fixMissing) then
      regular_ll=real(missingValue,kind=double)
    else
      regular_ll=0._double
    end if
!
!   Loop over points
!
    cnt=1_int32
    pnt=0_int32
    do i=1_int32,size(grid%pl)
      pl=>grid%pl(i)
      if (pl.eq.0_int32) cycle
      reducedLat=>grid%lat%dat(i)
      reducedLon=>grid%lon%dat(cnt:cnt+pl-1_int32)
      reducedDat=>reduced_ll(cnt:cnt+pl-1_int32)
      cnt=cnt+pl
! since lat(reduced_ll) == lat(regular_ll)
! --> only linear interpolation between longitudes!
      if (abs(abs(reducedLat)-90._double).lt.1e-8) then
        do ilon=1_int32,nlon
          pnt=pnt+1_int32
          regular_ll(pnt)=reducedDat(1_int32)
        end do
        cycle
      end if
      dpl=pl/360._double
      do ilon=1_int32,nlon
        pnt=pnt+1_int32
        a=floor(regularLon(ilon)*dpl)+1_int32
        if (a.eq.pl) then
          b=1_int32
        else
          b=a+1_int32
        end if
        call distance( reducedLat, regularLon(ilon), reducedLat, reducedLon(a), dax )
        if (dax.lt.1e-8) then
          regular_ll(pnt)=reducedDat(a)
          cycle
        end if
        if (fixMissing) then
          if (nint(reducedDat(a),int32).eq.missingValue) cycle
          if (nint(reducedDat(b),int32).eq.missingValue) cycle
        end if
        call distance( reducedLat, reducedLon(a), reducedLat, reducedLon(b), dab )
        regular_ll(pnt)=reducedDat(a)+(reducedDat(b)-reducedDat(a))*dax/dab
      end do
    end do     
!
    return
!
  End subroutine grib_2dfd_reduced_to_regular_ll
!
! =========================================================================================


! =========================================================================================
!
!   ..Subroutine grib_2dfd_reduced_spectrum_to_regular_ll
!
!       Make regular_ll grid of 2dfd data using bilinear interpolation.
!
!****************************
  Subroutine grib_2dfd_reduced_spectrum_to_regular_ll ( grid, reduced_ll, regular_ll, missingValue )
!****************************
!
    use math, only: distance
    implicit none
!
!   Dummy variables
!
    type(type_grib_grid)        , intent(in) , target       :: grid 
    real(double), dimension(:,:), intent(in) , target       :: reduced_ll
    real(double), dimension(:,:), intent(out), allocatable  :: regular_ll
    integer(int32)              , intent(in) , optional     :: missingValue
!
!   Local variables
!
    logical                  :: fixMissing
    integer(int32)           :: nlon, i, k, ilon, cnt, pnt, a, b, nfreq, npoints
    integer(int32), pointer  :: pl
    real(double)             :: dax, dab, dpl
    real(double), pointer    :: reducedLat
!
    real(double), dimension(nint((grid%lon%last-grid%lon%first)/&
      grid%lat%increment+1_int32,int32)), target  :: regularLon
    real(double), dimension(:)  , pointer      :: reducedLon
    real(double), dimension(:,:), pointer      :: reducedDat
!
!  ---
!
!   Check
!
    if (grid%gridType.ne.'reduced_ll') &
      stop 'Error @ grib_2dfd_reduced_spectrum_to_regular_ll : reduced_ll input grid expected!'
!
    fixMissing = present(missingValue)
!
!   New grid
!
    nlon = nint((grid%lon%last-grid%lon%first)/grid%lat%increment+1_int32,int32)
    regularLon = (/ (grid%lon%first+i*grid%lat%increment,i=0_int32,nlon-1_int32,1_int32) /)
    npoints=nlon*grid%lat%count
    nfreq = size(reduced_ll,2)
!
    if (allocated(regular_ll).and.size(regular_ll,1).ne.npoints.and.size(regular_ll,2).ne.nfreq) &
      deallocate(regular_ll)
    if (.not.allocated(regular_ll)) allocate(regular_ll(npoints,nfreq))
!
    if (fixMissing) then
      regular_ll=real(missingValue,kind=double)
    else
      regular_ll=0._double
    end if
!
!   Loop over points
!
    cnt=1_int32
    pnt=0_int32
    do i=1_int32,size(grid%pl)
      pl=>grid%pl(i)
      if (pl.eq.0_int32) cycle
      reducedLat=>grid%lat%dat(i)
      reducedLon=>grid%lon%dat(cnt:cnt+pl-1_int32)
      reducedDat=>reduced_ll(cnt:cnt+pl-1_int32,:)
      cnt=cnt+pl
! since lat(reduced_ll) == lat(regular_ll)
! --> only linear interpolation between longitudes!
      if (abs(abs(reducedLat)-90._double).lt.1e-8) then
        do ilon=1_int32,nlon
          pnt=pnt+1_int32
          regular_ll(pnt,:)=reducedDat(1_int32,:)
        end do
        cycle
      end if
      dpl=pl/360._double
      do ilon=1_int32,nlon
        pnt=pnt+1_int32
        a=floor(regularLon(ilon)*dpl)+1_int32
        if (a.eq.pl) then
          b=1_int32
        else
          b=a+1_int32
        end if
        call distance( reducedLat, regularLon(ilon), reducedLat, reducedLon(a), dax )
        if (dax.lt.1e-8) then
          regular_ll(pnt,:)=reducedDat(a,:)
          cycle
        end if
        call distance( reducedLat, reducedLon(a), reducedLat, reducedLon(b), dab )
        if (fixMissing) then
          do k=1_int32,nfreq
            if (nint(reducedDat(a,k),int32).eq.missingValue) cycle
            if (nint(reducedDat(b,k),int32).eq.missingValue) cycle
            regular_ll(pnt,k)=reducedDat(a,k)+(reducedDat(b,k)-reducedDat(a,k))*dax/dab
          end do
        else
          regular_ll(pnt,:)=reducedDat(a,:)+(reducedDat(b,:)-reducedDat(a,:))*dax/dab
        end if
      end do
    end do
!
    return
!
  End subroutine grib_2dfd_reduced_spectrum_to_regular_ll
!
! =========================================================================================
!
End module grib_2dfd
