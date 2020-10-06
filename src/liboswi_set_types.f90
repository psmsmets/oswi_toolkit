!*****************************************************************************80
!
!                                   L I B O S W I
!
!  Module:       LIBOSWI (include)
!
!  Programmer:   Pieter S. M. Smets
!                R&DSA depart. of Seismology and Acoustics
!                Koninklijk Nederlands Meteorologisch Instituut (KNMI)
!                De Bilt, The Netherlands
!
!  Date:         May 20, 2016
!
!  Language:     Fortran-90
!
!  Description:  liboswi set types functions and subroutines include
!
!
!*****************************************************************************80


Subroutine oswi_init ( oswi, clear, dealloc )
!*****************************************************************************80
!
!! OSWI_INIT
!
!  Description:
!
!    Initialize the entire oswi type structure. Optionally, clearing and
!    deallocating of the type can be disabled.  
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
!    Input/output, type ( type_oswi ) oswi.
!
!    Optional input, logical CLEAR, flag to toggle clearing of the type.
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout)  :: oswi
  logical, optional, intent(in)   :: clear, dealloc
!
! Local variables
!
  logical  :: doclear
!
! Init
!
  doclear=.true.
  if (present(clear)) doclear=clear
!
! hdr
!
  call oswi_init_hdr( oswi%hdr, clear, dealloc )
!
! dat
!
  if (doclear) call liboswi_clear_type_dat( oswi%dat, dealloc )
!
  return
!
!*****************************************************************************80
End subroutine oswi_init


Subroutine oswi_stack_init ( oswi_stack, clear, dealloc )
!*****************************************************************************80
!
!! OSWI_STACK_INIT
!
!  Description:
!
!    Initialize the entire oswi_stack type structure. Optionally, clearing and
!    deallocating of the type can be disabled.  
!
!  Modified:
!
!    21 June 2017
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_stack ) oswi_stack.
!
!    Optional input, logical CLEAR, flag to toggle clearing of the type.
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_stack), intent(inout)  :: oswi_stack
  logical, optional, intent(in)   :: clear, dealloc
!
! Local variables
!
  logical  :: doclear
!
! Init
!
  doclear=.true.
  if (present(clear)) doclear=clear
!
! hdr
!
  call oswi_init_hdr( oswi_stack%hdr, clear, dealloc )
!
! dat
!
  if (doclear) call oswi_stack_clear_dat( oswi_stack%dat, dealloc )
!
  return
!
!*****************************************************************************80
End subroutine oswi_stack_init


Subroutine oswi_init_hdr ( hdr, clear, dealloc )
!*****************************************************************************80
!
!! OSWI_INIT_HDR
!
!  Description:
!
!    Initialize the entire oswi type structure. Optionally, clearing and
!    deallocating of the type can be disabled.  
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
!    Input/output, type ( type_oswi_hdr ) hdr.
!
!    Optional input, logical CLEAR, flag to toggle clearing of the type.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr), intent(inout)  :: hdr
  logical, optional  , intent(in)     :: clear, dealloc
!
! Local variables
!
  logical  :: doclear
!
! Init
!
  doclear=.true.
  if (present(clear)) doclear=clear
!
! clear oswi input types
!
  if (doclear) call oswi_clear_hdr( hdr, dealloc )
!
! Set some stuff
!
  call oswi_init_env ( hdr%env )
!
  hdr%history=''
  hdr%type='swi'
  hdr%Pa=.false.
  hdr%dB=.false.
  hdr%lg=.true.
  hdr%f%first=-1._double
  hdr%f%last=-1._double
  hdr%dem%resampleMethod=''
  hdr%dem%defined=.true.
  hdr%osw%hasselmann=.true.
  hdr%integrate=.false.
  hdr%fanalysis=.false.
  hdr%missingValue=-9999_int32
!
  return
!
!*****************************************************************************80
End subroutine oswi_init_hdr


Subroutine oswi_set_output ( oswi )
!*****************************************************************************80
!
!! OSWI_SET_OUTPUT
!
!  Description:
!
!    Initialize the oswi hdr output information for the defined output type.  
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
!    Input/output, type ( type_oswi ) oswi.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout)  :: oswi
!
  call oswi_set_output_hdr( oswi%hdr )
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_output


Subroutine oswi_set_output_hdr ( hdr )
!*****************************************************************************80
!
!! OSWI_SET_OUTPUT_HDR
!
!  Description:
!
!    Initialize the oswi hdr output information for the defined output type.  
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
!    Input/output, type ( type_oswi_hdr ) hdr.
!
!
!****************************
!
  use string_utility, only: strlowcase
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr), intent(inout), target  :: hdr
!
! Local variables
!
  character(len=10)  :: pref
!
! Update all
!
  hdr%air = .false.
  hdr%sea = .false.
  hdr%bed = .false.
  hdr%swh = .false.
  hdr%swi = .false.
  hdr%sfd = .false.
  hdr%Pa  = .false.
!
  hdr%type=strlowcase(hdr%type)
!
  select case ( hdr%type )
    case ('dem')
      hdr%name          = 'depth'
      hdr%long_name     = 'Sea floor depth'
      hdr%standard_name = 'sea_floor_depth'
      hdr%units         = 'm'
      return
    case ('air')
      hdr%air           = .true.
      hdr%Pa            = .true.
    case ('sea')
      hdr%sea           = .true.
      hdr%Pa            = .true.
    case ('bed')
      hdr%bed           = .true.
      hdr%Pa            = .true.
    case ('swh')
      hdr%swh           = .true.
      hdr%name          = 'swh'
      hdr%long_name     = 'Sea surface wave significant height'
      hdr%standard_name = 'sea_surface_wave_significant_height'
      hdr%units         = 'm'
      if (hdr%variance) hdr%units = 'm2'
    case ('swi')
      hdr%swi           = .true.
      hdr%name          = 'swi'
      hdr%long_name     = 'Sea surface wave interaction'
      hdr%standard_name = 'sea_surface_wave_interaction' ! _spectral_density
      hdr%units         = 'm2'
      if (hdr%variance) hdr%units = 'm4'
    case ('sfd')
      hdr%sfd           = .true.
      hdr%name          = 'd'
      hdr%long_name     = 'Sea floor displacement'
      hdr%standard_name = 'sea_floor_displacement'
      hdr%units         = 'm'
      if (hdr%variance) hdr%units = 'm2'
    case default
      hdr%type          = 'swi'
      hdr%swi           = .true.
      hdr%Pa            = .false.
  end select
  hdr%osw%hasselmann = .not.hdr%swh
!
! Acoustic pressure in air/sea/bedrock?
!
  if (hdr%Pa) then
    hdr%name             = 'p'
    hdr%units            = 'Pa'
    hdr%long_name        = 'Equivalent sound pressure variation'
    if (hdr%debug) hdr%long_name = 'Sound pressure'
    hdr%standard_name    = 'sound_pressure'
    if (.not.hdr%integrate) then
      hdr%long_name     = trim(hdr%long_name) // ' spectral density'
      hdr%standard_name = trim(hdr%standard_name) // '_spectral_density'
    endif
    if (hdr%air) then
      hdr%referencePressure=20_int32 ! 20µPa
      hdr%name          = trim(hdr%name)//'_air'
      hdr%long_name     = trim(hdr%long_name) // ' in air'
      hdr%standard_name = trim(hdr%standard_name) // '_in_air'
    elseif (hdr%sea) then
      hdr%referencePressure=1_int32 ! 1µPa
      hdr%name          = trim(hdr%name)//'_sea'
      hdr%long_name     = trim(hdr%long_name) // ' in sea water'
      hdr%standard_name = trim(hdr%standard_name) // '_in_sea_water'
    elseif (hdr%bed) then
      hdr%referencePressure=1_int32 ! 1µPa
      hdr%name          = trim(hdr%name)//'_bedrock'
      hdr%long_name     = trim(hdr%long_name) // ' in bedrock'
      hdr%standard_name = trim(hdr%standard_name) // '_in_bedrock'
    endif
    if (hdr%dB.and.hdr%lg) hdr%dB=.false.
    if (hdr%dB) hdr%variance=.false.
    if (hdr%variance) then
      if (hdr%integrate) then
        hdr%units = 'Pa2'
      else
        hdr%units = 'Pa2 Hz-1'
      endif
    else
      if (hdr%integrate) then
        hdr%units = 'Pa'
      else
        hdr%units = 'Pa2 Hz-1/2'
      endif
    endif
    if (hdr%lg) hdr%units = 'log('//trim(hdr%units)//')'
    if (hdr%dB) then
      if (hdr%integrate) then
        hdr%units = 'dB'
      else
        hdr%units = 'dB Hz-1/2'
      endif
      if ( hdr%referencePressure.lt.100_int32) then
        write (pref,'(i6)') hdr%referencePressure
        hdr%units = trim(hdr%units)//' re '//trim(adjustl(pref))//'µPa'
      else
        write (pref,'(i6)') int(hdr%referencePressure*1.d-6,kind=int32)
        hdr%units = trim(hdr%units)//' re '//trim(adjustl(pref))//'Pa'
      endif
    endif
  else
    hdr%referencePressure=nint(1.d+6,kind=int32)
    hdr%dB = .false.
    hdr%lg = .false.
    if (.not.hdr%integrate) then
      hdr%long_name     = trim(hdr%long_name) // ' spectral density'
      hdr%standard_name = trim(hdr%standard_name) // '_spectral_density'
      if (hdr%variance) then
        hdr%units      = trim(hdr%units) // ' Hz-1'
      else
        hdr%units      = trim(hdr%units) // ' Hz-1/2'
      endif
    endif
  endif
!
  if (hdr%debug) then
    hdr%dB=.false.
    hdr%Pa=.false.
    hdr%Lg=.false.
    hdr%name='mod_coeff'
    hdr%long_name='Modulation coefficient of ' // strlowcase(trim(hdr%long_name))
    hdr%standard_name=''
    hdr%units='-'
  endif
!
  if (hdr%normalize) then
    hdr%long_name        = 'Normalized ' // trim(strlowcase(hdr%long_name))
    hdr%standard_name    = 'normalized_' // trim(hdr%standard_name)
    hdr%units            = '-'
  endif
!
  if (hdr%variance) then
    hdr%long_name        = 'Square of ' // trim(strlowcase(hdr%long_name))
    hdr%standard_name    = 'square_of_' // trim(hdr%standard_name)
  endif
!
  if (hdr%swh.or.hdr%swi) then
    hdr%f%dat=>hdr%f%wave
  else
    hdr%f%dat=>hdr%f%sound
  end if
!
! set frequency range
!
!!  if (hdr%swh) then
!!    if ((hdr%f%first+1._double).gt.1.d-8) hdr%f%first=hdr%f%first/2
!!    if ((hdr%f%last +1._double).gt.1.d-8) hdr%f%last =hdr%f%last/2
!!  endif
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_output_hdr


Subroutine oswi_init_env ( env )
!*****************************************************************************80
!
!! OSWI_INIT_ENV
!
!  Description:
!
!    Initialize the entire oswi environment type structure. 
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
!    Input/output, type ( type_oswi_env ) env, oswi environment type.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_env), intent(inout), target  :: env
!
!! dem%bathymetry%standard_name = 'sea_floor_depth_below_sea_level'
!
  env%rho_air%name          = 'rho_air'
  env%rho_air%long_name     = 'Atmospheric density of air at sea level (Internatial Standard Atmosphere)'
  env%rho_air%standard_name = 'air_density'
  env%rho_air%units         = 'kg m-3'
  env%rho_air%value         =    1.2466_double
!
  env%rho_sea%name          = 'rho_sea'
  env%rho_sea%long_name     = 'Sea water density'
  env%rho_sea%standard_name = 'sea_water_density'
  env%rho_sea%units         = 'kg m-3'
  env%rho_sea%value         = 1000.0000_double
!
  env%rho_bed%name          = 'rho_bedrock'
  env%rho_bed%long_name     = 'Bedrock density'
  env%rho_bed%standard_name = 'bedrock_density'
  env%rho_bed%units         = 'kg m-3'
  env%rho_bed%value         = 2700.0000_double
!
  env%c_air%name            = 'c_air'
  env%c_air%long_name       = 'Speed of sound in air at sea level (Internatial Standard Atmosphere)'
  env%c_air%standard_name   = 'speed_of_sound_in_air'
  env%c_air%units           = 'm s-1'
  env%c_air%value           =  337.3100_double ! velocity of atmosphere in m/s
!
  env%c_sea%name            = 'c_sea'
  env%c_sea%long_name       = 'Speed of sound in sea water'
  env%c_sea%standard_name   = 'speed_of_sound_in_sea_water'
  env%c_sea%units           = 'm s-1'
  env%c_sea%value           = 1500.0000_double
!
  env%c_bed%name            = 'c_bedrock'
  env%c_bed%long_name       = 'Shear wave velocity in bedrock' ! Jensen et al., 2011, pg. 43!
  env%c_bed%standard_name   = 'bedrock_shear_wave_velocity'
  env%c_bed%units           = 'm s-1'
  env%c_bed%value           = 2500.0000_double
!
  return
!
!*****************************************************************************80
End subroutine oswi_init_env


Subroutine oswi_set_env ( env, rho_air, c_air, rho_sea, c_sea, &
   rho_bed, c_bed )
!*****************************************************************************80
!
!! OSWI_SET_ENV
!
!  Description:
!
!    Set/update oswi environment variable value. 
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
!    Input/output, type ( type_oswi_env ) env, oswi environment type.
!
!    Optional input, real ( double ) rho_air, air density.
!
!    Optional input, real ( double ) rho_sea, sea water density.
!
!    Optional input, real ( double ) rho_bed, bedrock density.
!
!    Optional input, real ( double ) c_air, speed of sound in air.
!
!    Optional input, real ( double ) c_sea, speed of sound in sea water.
!
!    Optional input, real ( double ) c_bed, bedrock shear velocity.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_env), intent(inout), target  :: env
  real(double), intent(in), optional  :: rho_air, rho_sea, rho_bed
  real(double), intent(in), optional  :: c_air, c_sea, c_bed
!
  if (present(rho_air)) env%rho_air%value = rho_air
  if (present(rho_sea)) env%rho_sea%value = rho_sea
  if (present(rho_bed)) env%rho_bed%value = rho_bed
!
  if (present(c_air)) env%c_air%value = c_air
  if (present(c_sea)) env%c_sea%value = c_sea
  if (present(c_bed)) env%c_bed%value = c_bed
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_env


Subroutine oswi_set_grid ( oswi, grid )
!*****************************************************************************80
!
!! OSWI_SET_GRID
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
  endif
  call oswi_set_grid_ll ( oswi%lat, grid%lat )
  call oswi_set_grid_ll ( oswi%lon, grid%lon )
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_grid


Subroutine oswi_set_grid_ll ( oswi, ll )
!*****************************************************************************80
!
!! OSWI_SET_GRID_LL
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
  type(type_grib_grid_ll), intent(in   )  :: ll
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
     endif
     if (allocated(ll%mask)) then
        allocate(oswi%mask(oswi%size))
    oswi%mask=ll%mask
     endif
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_grid_ll


Subroutine oswi_unpack_grid ( grid, verbose )
!*****************************************************************************80
!
!! OSWI_UNPACK_GRID
!
!  Description:
!
!    Unpack unique grid to match data (numberOfPoints) vector. 
!
!  Modified:
!
!    22 August 2017
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_grid ) grid.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid), intent(inout), target  :: grid
  logical, intent(in), optional  :: verbose
!
! Local variables
!
  logical :: verb
  integer(int32)           :: cnt, ilat, ilon
  integer(int32), pointer  :: pl
  real(double)  , pointer  :: lat, lon
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write(output_unit,"(2x,'>> ',a)") 'Unpack grid coordinates'
!
  if (grid%gridType.eq.'reduced_ll') then
    if (allocated(grid%lat%dat_unpacked).and.size(grid%lat%dat_unpacked).ne.grid%numberOfPoints) &
      deallocate(grid%lat%dat_unpacked)
    if (.not.allocated(grid%lat%dat_unpacked)) allocate(grid%lat%dat_unpacked(grid%numberOfPoints))
    cnt=0_int32
    do ilat=1_int32,grid%lat%size
      pl=>grid%pl(ilat)
      lat=>grid%lat%dat(ilat)
      if (pl.eq.0_int32) cycle
      grid%lat%dat_unpacked(cnt+1_int32:cnt+pl)=lat
      cnt=cnt+pl
    enddo
    if (grid%numberOfPoints.ne.cnt) stop 'Error @ oswi_unpack_grid'
  elseif (grid%gridType.eq.'regular_ll') then
    if (allocated(grid%lat%dat_unpacked).and.size(grid%lat%dat_unpacked).ne.grid%numberOfPoints) &
      deallocate(grid%lat%dat_unpacked)
    if (.not.allocated(grid%lat%dat_unpacked)) allocate(grid%lat%dat_unpacked(grid%numberOfPoints))
    if (allocated(grid%lon%dat_unpacked).and.size(grid%lon%dat_unpacked).ne.grid%numberOfPoints) &
      deallocate(grid%lon%dat_unpacked)
    if (.not.allocated(grid%lon%dat_unpacked)) allocate(grid%lon%dat_unpacked(grid%numberOfPoints))
    cnt=0_int32
    do ilat=1_int32,grid%lat%size
      lat=>grid%lat%dat(ilat)
      do ilon=1_int32,grid%lon%size
        lon=>grid%lon%dat(ilon)
        cnt=cnt+1_int32
        grid%lon%dat_unpacked(cnt)=lon
        grid%lat%dat_unpacked(cnt)=lat
      enddo
    enddo
  else
    if (allocated(grid%lat%dat_unpacked)) deallocate(grid%lat%dat_unpacked)
    if (allocated(grid%lon%dat_unpacked)) deallocate(grid%lon%dat_unpacked)
  endif
!
  return
!
!*****************************************************************************80
End subroutine oswi_unpack_grid


Subroutine oswi_set_f ( oswi, f )
!*****************************************************************************80
!
!! OSWI_SET_F
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
real(double) :: tmp
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
End subroutine oswi_set_f


Subroutine oswi_set_osw ( oswi, grib_2dfd )
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
  type(type_oswi)     , intent(inout)  :: oswi
  type(type_grib_2dfd), intent(in   )  :: grib_2dfd
!
! ----
!
  oswi%hdr%osw%shortName = grib_2dfd%shortName
  oswi%hdr%osw%units = grib_2dfd%units
  oswi%hdr%osw%centre = grib_2dfd%centre
  oswi%hdr%osw%dataClass = grib_2dfd%dataClass
  oswi%hdr%osw%dataType = grib_2dfd%dataType
  oswi%hdr%osw%dataStream = grib_2dfd%dataStream
  oswi%hdr%osw%gridType = grib_2dfd%grid%gridType
  oswi%hdr%osw%paramId = grib_2dfd%paramId
  oswi%hdr%osw%experimentVersionNumber = grib_2dfd%experimentVersionNumber
  oswi%hdr%osw%editionNumber = grib_2dfd%editionNumber
  oswi%hdr%osw%frequencies = grib_2dfd%frequency%size
  oswi%hdr%osw%firstFrequency = grib_2dfd%frequency%dat(1)
  oswi%hdr%osw%frequencyScalar = 1.1_double
  oswi%hdr%osw%directions = grib_2dfd%direction%size
  oswi%hdr%osw%directionIncrement = grib_2dfd%direction%increment
  oswi%hdr%osw%directionFullCircle = grib_2dfd%direction%fullCircle
  if (grib_2dfd%number%defined) oswi%hdr%osw%ensembleNumber = grib_2dfd%number%list(grib_2dfd%number%sel)
  if (grib_2dfd%step%defined) oswi%hdr%osw%forecastStep = grib_2dfd%step%list(grib_2dfd%step%sel)
!
!  oswi%dat%missingValue=-9999_int32
!
  call oswi_set_grid( oswi%hdr%grid, grib_2dfd%grid )
  call oswi_set_f( oswi%hdr%f, grib_2dfd%frequency )
!
  call liboswi_allocate_type_dat( oswi%hdr, oswi%dat, fill=.true., both=.true. )
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_osw


Subroutine oswi_set_dem ( oswi, dem )
!*****************************************************************************80
!
!! OSWI_SET_DEM
!
!  Description:
!
!    Set oswi bathymetry type. 
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
!    Input/output, type ( type_oswi_dem ) oswi, oswi dem type.
!
!    Input, type ( type_dem ), dem, dem type
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_dem), intent(inout)  :: oswi
  type(type_dem)     , intent(in   )  :: dem
!
! ----
!
  oswi%defined = .true.
  oswi%node_offset = dem%attributes%node_offset
  oswi%units = dem%elev%units
  oswi%title = dem%attributes%title
  oswi%institution = dem%attributes%institution
  oswi%source = dem%attributes%source
  oswi%history = dem%attributes%history
  oswi%references = dem%attributes%references
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_dem


Subroutine oswi_set_frequency_range ( f, indexOfFirstFrequency, indexOfLastFrequency, &
  firstFrequency, lastFrequency, mask, sound )
!*****************************************************************************80
!
!! OSWI_SET_FREQUENCY_RANGE
!
!  Description:
!
!    Set oswi frequency range given various range definitions (index, value, mask). 
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
!    Input/output, type ( type_oswi_f ) f, oswi frequency type.
!
!    Optional input, integer(int32) indexOfFirstFrequency, index of first/lower frequency
!
!    Optional input, integer(int32) indexOfLastFrequency, index of last/upper frequency
!
!    Optional input, real(double) firstFrequency, first/lower frequency
!
!    Optional input, real(double) lastFrequency, last/upper frequency
!
!    Optional input, logical vector mask, mask vector of dimension f%size
!
!    Optional input, logical sound, enable acoustic frequency instead of surface wave frequency
!
!
!****************************
!

!
  implicit none
!
! Dummy variables
!
  type(type_oswi_f)         , intent(inout), target    :: f 
  integer(int32)            , intent(in)   , optional  :: indexOfFirstFrequency, indexOfLastFrequency
  real(double)              , intent(in)   , optional  :: firstFrequency, lastFrequency
  logical, dimension(f%size), intent(in)   , optional  :: mask
  logical                   , intent(in)   , optional  :: sound
!
! Local variables
!
  logical                 :: sound_
  integer(int32), pointer :: ixFirst, ixLast
!
  sound_ = .false.
  if (present(sound)) sound_=sound
!
  ixFirst => f%ixFirst
  ixLast  => f%ixLast
!
  if (sound_) then
    f%dat=>f%sound
  else
    f%dat=>f%wave
  end if
!
! Different io options
!
  if (present(mask)) then
    f%mask=mask
    ixFirst=-1_int32
    ixLast=-1_int32
  else if( present(indexOfFirstFrequency).and.present(indexOfLastFrequency) ) then
    if(indexOfFirstFrequency.lt.1_int32.or.indexOfFirstFrequency.gt.f%size.or.&
      indexOfLastFrequency .lt.1_int32.or.indexOfLastFrequency .gt.f%size) &
      stop 'Error @ oswi_set_frequency_range : illegal indexes given!'
    f%mask=.false.
    f%mask(indexOfFirstFrequency:indexOfLastFrequency)=.true.
    ixFirst=indexOfFirstFrequency
    ixLast=indexOfLastFrequency
  else if( present(firstFrequency).and.present(lastFrequency) ) then
    if (abs(firstFrequency+1._double).lt.1e-8) then
      ixFirst=1_int32
    else
      if (sound_) then
        ixFirst=minloc(abs(f%sound-FirstFrequency),1,f%sound-FirstFrequency.lt.0._double)
      else
        ixFirst=minloc(abs(f%wave-FirstFrequency),1,f%wave-FirstFrequency.lt.0._double)
      endif
      if (ixFirst.lt.1_int32) ixFirst=1_int32
      if (ixFirst.gt.f%size) ixFirst=f%size
    endif
    if (abs(lastFrequency+1._double).lt.1e-8) then
      ixLast=f%size
    else
      if (sound_) then
        ixLast=minloc(abs(LastFrequency-f%sound),1,LastFrequency-f%sound.gt.0._double)+1_int32
      else
        ixLast=minloc(abs(LastFrequency-f%wave),1,LastFrequency-f%wave.gt.0._double)+1_int32
      endif
      if (ixLast.gt.f%size) ixLast=f%size
      if (ixLast.lt.1_int32) ixLast=1_int32
    endif
    f%mask=.false.
    f%mask(ixFirst:ixLast)=.true.
  else
    stop 'Error @ oswi_set_frequency_range : either mask vector, indexes or values expected!'
  endif
!
  f%count=count(f%mask)
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_frequency_range
