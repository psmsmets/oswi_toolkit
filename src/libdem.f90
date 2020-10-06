!*****************************************************************************80
!
!                                   L I B D E M
!
!  Module:       LIBDEM
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
!  Description:  This module includes several subroutines to import and handles
!                DEM NetCDF files.
!
!
!*****************************************************************************80

Module libdem

  use string_utility, only: strlowcase
  use io_types
  use math
  use netcdf
  use netcdf_ext

  implicit none

!
!..parameters
!
   integer(int32), parameter, private  :: len_att    = 80
   integer(int32), parameter, private  :: len_att_gl = 2048
!
!..define dem types
!
  type type_dem_dim
    logical                      :: global
    integer(int32)               :: dimid, varid
    character(len=len_att)       :: name, long_name, standard_name, units, var_name, axis
    real(double)                 :: step, minval, maxval, range, offset
    integer(int32)               :: count 
    real(double), dimension(:), allocatable  :: dat 
  end type type_dem_dim
!
  type type_dem_var
    integer(int32)               :: varid
    integer(int32), dimension(2) :: dimids
    character(len=len_att)       :: name, long_name, standard_name, units, var_name
    real(double)                 :: scale_factor, add_offset, missing_value, fillValue, valid_min, valid_max
    real(single)                 :: minval, maxval, range
    real(single), dimension(:,:), allocatable  :: dat
  end type type_dem_var
!
  type type_dem_att
    character(len=len_att_gl)    :: Conventions, title, institution, source, history, references, comment
    logical                      :: node_offset
  end type type_dem_att
!
!
  type type_dem
    integer(int32)               :: ncid
    character(len=len_att_gl)    :: file
    type(type_dem_dim)           :: lon, lat
    type(type_dem_var)           :: elev 
    type(type_dem_att)           :: attributes 
  end type type_dem
!
!
  type type_block_ptr
    real(single), dimension(:,:), pointer  :: dat
  end type type_block_ptr

Contains

! =========================================================================================
!
!.......Subroutine set_globeranges 
!
!       Calculate the frequency based on the start frequency and the 2DFD index
!
!****************************
  Subroutine import_dem ( file, dem, verbose, deallocate_dims, flip, clip, modulus, siunits )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    character(len=*)              , intent(in)               :: file
    type(type_dem)                , intent(out)              :: dem
    logical                       , intent(in)   , optional  :: verbose, deallocate_dims, flip, clip, modulus, siunits
!
!.....Local variables
!
    logical           :: verb, dealloc_dims, doflip, doclip, domodulus, dosiUnits
    integer(int32)    :: ncid
!
!  ---
!
!.....init
!
    verb=.false.
    if(present(verbose))verb=verbose
!
    dealloc_dims=.false.
    if(present(deallocate_dims))dealloc_dims=deallocate_dims
!
    doflip=.false.
    if(present(flip))doflip=flip
!
    doclip=.false.
    if(present(clip))doclip=clip
!
    domodulus=.false.
    if(present(modulus))domodulus=modulus
!
    dosiunits=.false.
    if(present(siunits))dosiunits=siunits
!
    call clear_dem( dem )
!
!.....Open dem file
!
    call nc_check( nf90_open( file, nf90_nowrite, ncid ) )
!
!.....Set file name
!
    dem%file = trim(file)
!
!.....Get lon/lat dimensions
!
    call get_dem_dims( ncid, dem )
!
!.....Get dem variables 
!
    call get_dem_vars( ncid, dem )
!
!.....Get global attributes
!
    call get_dem_global_attributes( ncid, dem )
!
!.....Get dem data 
!
    call get_dem_data( ncid, dem )
!
!.....Convert to SI units
!
    if (len_trim(dem%elev%units).eq.0_int32) dem%elev%units = 'm'
    if (dosiunits) then
      select case (strlowcase(dem%elev%units))
        case ('m','meter','meters')
          dem%elev%units = 'm'
        case ('km','kilometer','kilometers')
          dem%elev%dat=dem%elev%dat/1000
          dem%elev%units = 'm'
        case default
          stop 'Error @ import_dem : Unknown elevation units given!'
      end select
    end if
!
!.....Clear data vectors dimensions?
!
    if (dealloc_dims) deallocate( dem%lon%dat, dem%lat%dat )
!
!.....Flip elevation sign
!
    if (domodulus) dem%elev%dat=abs(dem%elev%dat)
    if (doflip) dem%elev%dat=-dem%elev%dat
    if (doclip) then
      where (dem%elev%dat.lt.0._single)
        dem%elev%dat=0._single
      end where
    end if
!
!.....Print
!
    if (verb) call print_dem_info( dem )
!
!.....Close dem file
!
    call nc_check( nf90_close( ncid ) )
!
    Return
!
  End subroutine import_dem
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine set_globeranges 
!
!       Calculate the frequency based on the start frequency and the 2DFD index
!
!****************************
  Subroutine write_dem ( file, dem, verbose )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    character(len=*)              , intent(in)               :: file
    type(type_dem)                , intent(inout)            :: dem
    logical                       , intent(in)   , optional  :: verbose
!
!.....Local variables
!
    logical                    :: verb
    integer(int32)             :: ncid, i
    integer(int32), parameter  :: shuffle = 1_int32 ! If non-zero, turn on the shuffle filter.
    integer(int32), parameter  :: deflate = 1_int32 ! If non-zero, turn on deflate specified by level.
    integer(int32), parameter  :: level   = 5_int32 ! Set deflate level [1,9]
!
!  ---
!
!.....init
!
    verb=.false.
    if(present(verbose))verb=verbose
!
!.....Verbose
!
    if (verb) print "(2A)", "Write dem data to ", file
    verb=.true. ! force to show errors
!
!.....Create file
!
    call nc_check( nf90_create( path=file, cmode=NF90_NETCDF4, ncid=ncid ), verb )
!
!.....Define the dimensions
!
    call nc_check( nf90_def_dim( ncid, trim(dem%lon%name), dem%lon%count, dem%lon%dimid ), verb )
    call nc_check( nf90_def_dim( ncid, trim(dem%lat%name), dem%lat%count, dem%lat%dimid ), verb )
!
!.....Define the variables
!
    call nc_check( nf90_def_var( ncid, trim(dem%lon%name), nf90_double, dem%lon%dimid, dem%lon%varid ), verb )
    call nc_check( nf90_def_var( ncid, trim(dem%lat%name), nf90_double, dem%lat%dimid, dem%lat%varid ), verb )
    call nc_check( nf90_def_var( ncid, trim(dem%elev%name), nf90_float, &
      (/dem%lon%dimid,dem%lat%dimid/), dem%elev%varid ), verb )
!
!.....Define variables deflation
!
    call nc_check( nf90_def_var_deflate ( ncid, dem%lon%varid , shuffle, deflate, level ) )
    call nc_check( nf90_def_var_deflate ( ncid, dem%lat%varid , shuffle, deflate, level ) )
    call nc_check( nf90_def_var_deflate ( ncid, dem%elev%varid, shuffle, deflate, level ) )
!
!.....Add the variable attributes
!
    call nc_check( nf90_put_att( ncid, dem%lon%varid, 'long_name', trim(dem%lon%long_name) ), verb )
    call nc_check( nf90_put_att( ncid, dem%lon%varid, 'standard_name', trim(dem%lon%standard_name) ), verb )
    call nc_check( nf90_put_att( ncid, dem%lon%varid, 'units', trim(dem%lon%units) ), verb )
    call nc_check( nf90_put_att( ncid, dem%lon%varid, 'axis', trim(dem%lon%axis) ), verb )
    call nc_check( nf90_put_att( ncid, dem%lon%varid, 'valid_range', & 
      (/ dem%lon%minval-dem%lon%offset, dem%lon%maxval+dem%lon%offset /) ) )
    call nc_check( nf90_put_att( ncid, dem%lon%varid, 'scale_factor', 1._double ) )
    call nc_check( nf90_put_att( ncid, dem%lon%varid, 'add_offset', 0._double ) )
!
    call nc_check( nf90_put_att( ncid, dem%lat%varid, 'long_name', trim(dem%lat%long_name) ), verb )
    call nc_check( nf90_put_att( ncid, dem%lat%varid, 'standard_name',trim(dem%lat%standard_name) ), verb )
    call nc_check( nf90_put_att( ncid, dem%lat%varid, 'units', trim(dem%lat%units) ), verb )
    call nc_check( nf90_put_att( ncid, dem%lat%varid, 'axis', trim(dem%lat%axis) ), verb )
    call nc_check( nf90_put_att( ncid, dem%lat%varid, 'valid_range', &
      (/ dem%lat%minval-dem%lat%offset, dem%lat%maxval+dem%lat%offset /) ) )
    call nc_check( nf90_put_att( ncid, dem%lat%varid, 'scale_factor', 1._double ) )
    call nc_check( nf90_put_att( ncid, dem%lat%varid, 'add_offset', 0._double ) )
!
    call nc_check( nf90_put_att( ncid, dem%elev%varid, 'long_name', trim(dem%elev%long_name) ), verb )
    call nc_check( nf90_put_att( ncid, dem%elev%varid, 'standard_name',trim(dem%elev%standard_name) ), verb )
    call nc_check( nf90_put_att( ncid, dem%elev%varid, 'units', trim(dem%elev%units) ), verb )
    call nc_check( nf90_put_att( ncid, dem%elev%varid, 'valid_range', (/ dem%elev%minval, dem%elev%maxval /) ) )
    call nc_check( nf90_put_att( ncid, dem%elev%varid, 'scale_factor', dem%elev%scale_factor ) )
    call nc_check( nf90_put_att( ncid, dem%elev%varid, 'add_offset', dem%elev%add_offset ) )
!
!.....Add the global attributes
!
    if (len_trim(dem%attributes%Conventions).gt.0_int32) &
      call nc_check( nf90_put_att( ncid, NF90_GLOBAL, 'Conventions', trim(dem%attributes%Conventions) ), verb )
    if (len_trim(dem%attributes%title).gt.0_int32) &
      call nc_check( nf90_put_att( ncid, NF90_GLOBAL, 'title', trim(dem%attributes%title) ), verb )
    if (len_trim(dem%attributes%institution).gt.0_int32) &
      call nc_check( nf90_put_att( ncid, NF90_GLOBAL, 'institution', trim(dem%attributes%institution) ), verb )
    if (len_trim(dem%attributes%source).gt.0_int32) &
      call nc_check( nf90_put_att( ncid, NF90_GLOBAL, 'source', trim(dem%attributes%source) ), verb )
    if (len_trim(dem%attributes%history).gt.0_int32) &
      call nc_check( nf90_put_att( ncid, NF90_GLOBAL, 'history', trim(dem%attributes%history) ), verb )
    if (len_trim(dem%attributes%references).gt.0_int32) &
      call nc_check( nf90_put_att( ncid, NF90_GLOBAL, 'references', trim(dem%attributes%references) ), verb )
    if (len_trim(dem%attributes%comment).gt.0_int32) &
      call nc_check( nf90_put_att( ncid, NF90_GLOBAL, 'comment', trim(dem%attributes%comment) ), verb )
!
    if (dem%attributes%node_offset) then
      call nc_check( nf90_put_att( ncid, NF90_GLOBAL, 'node_offset', 1_int32 ), verb )
    else
      call nc_check( nf90_put_att( ncid, NF90_GLOBAL, 'node_offset', 0_int32 ), verb )
    end if
!
!.....End definitions
!
    call nc_check( nf90_enddef(ncid), verb )
!
!.....Put the data
!
    if (.not.allocated(dem%lon%dat)) then
      allocate(dem%lon%dat(dem%lon%count))
      dem%lon%dat=(/ ( dem%lon%minval+(i-1_int32)*dem%lon%step, i=1_int32, dem%lon%count ) /)
    end if
    call nc_check( nf90_put_var( ncid, dem%lon%varid, dem%lon%dat ) )
!
    if (.not.allocated(dem%lat%dat)) then
      allocate(dem%lat%dat(dem%lat%count))
      dem%lat%dat=(/ ( dem%lat%minval+(i-1_int32)*dem%lat%step, i=1_int32, dem%lat%count ) /)
    end if
    call nc_check( nf90_put_var( ncid, dem%lat%varid, dem%lat%dat ) )
!
    call nc_check( nf90_put_var( ncid, dem%elev%varid, dem%elev%dat ) )
!
!.....Close dem file
!
    call nc_check( nf90_close(ncid), verb )
!
    Return
!
  End subroutine write_dem
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine get_dem_dims 
!
!       Show dem info
!
!****************************
  Subroutine print_dem_info ( dem )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_dem), intent(in)  :: dem
!
!  ---
    print "(2a)", 'netCDF DEM file ', trim(dem%file) 
    print "(a)" , 'dimensions:' 
    print "(5x,a,i6)" , 'lon = ', dem%lon%count
    print "(5x,a,i6)" , 'lat = ', dem%lat%count
!
    print "(a)", 'variables :'
!
    print "(5x,a)" , 'lon:'
    if(len_trim(dem%lon%long_name).gt.0) &
      print "(10x,2a)" , 'lon: long_name = ', trim(dem%lon%long_name)
    if(len_trim(dem%lon%standard_name).gt.0) &
      print "(10x,2a)" , 'lon: standard_name = ', trim(dem%lon%standard_name)
    if(len_trim(dem%lon%units).gt.0) &
      print "(10x,2a)" , 'lon: units = ', trim(dem%lon%units)
    if(len_trim(dem%lon%var_name).gt.0) &
      print "(10x,2a)" , 'lon: var_name = ', trim(dem%lon%var_name)
    if(len_trim(dem%lon%axis).gt.0) &
    print "(10x,2a)" , 'lon: axis = ', trim(dem%lon%axis)
    print "(10x,a,f9.3)" , 'lon: min = ', dem%lon%minval-dem%lon%offset
    print "(10x,a,f9.3)" , 'lon: max = ', dem%lon%maxval+dem%lon%offset
    print "(10x,a,f9.3)" , 'lon: range = ', dem%lon%range
    print "(10x,a,f9.6)" , 'lon: step = ', dem%lon%step
!
    print "(5x,a)" , 'lat:'
    if(len_trim(dem%lat%long_name).gt.0) &
      print "(10x,2a)" , 'lat: long_name = ', trim(dem%lat%long_name)
    if(len_trim(dem%lat%standard_name).gt.0) &
      print "(10x,2a)" , 'lat: standard_name = ', trim(dem%lat%standard_name)
    if(len_trim(dem%lat%units).gt.0) &
      print "(10x,2a)" , 'lat: units = ', trim(dem%lat%units)
    if(len_trim(dem%lat%var_name).gt.0) &
      print "(10x,2a)" , 'lat: var_name = ', trim(dem%lat%var_name)
    if(len_trim(dem%lat%axis).gt.0) &
    print "(10x,2a)" , 'lat: axis = ', trim(dem%lat%axis)
    print "(10x,a,f9.3)" , 'lat: min = ', dem%lat%minval-dem%lat%offset
    print "(10x,a,f9.3)" , 'lat: max = ', dem%lat%maxval+dem%lat%offset
    print "(10x,a,f9.3)" , 'lat: range = ', dem%lat%range
    print "(10x,a,f9.6)" , 'lat: step = ', dem%lat%step
!
    print "(5x,a)" , 'elevation:'
    if(len_trim(dem%elev%long_name).gt.0) &
      print "(10x,2a)" , 'elev: long_name = ', trim(dem%elev%long_name)
    if(len_trim(dem%elev%standard_name).gt.0) &
      print "(10x,2a)" , 'elev: standard_name = ', trim(dem%elev%standard_name)
    if(len_trim(dem%elev%units).gt.0) &
      print "(10x,2a)" , 'elev: units = ', trim(dem%elev%units)
    if(len_trim(dem%elev%var_name).gt.0) &
      print "(10x,2a)" , 'elev: var_name = ', trim(dem%elev%var_name)
    print "(10x,a,f9.1)" , 'elev: min = ', dem%elev%minval
    print "(10x,a,f9.1)" , 'elev: max = ', dem%elev%maxval
    print "(10x,a,f9.1)" , 'elev: range = ', dem%elev%range
!
    print "(a)", '// global attributes :'
    if(len_trim(dem%attributes%Conventions).gt.0) &
      print "(10x,2a)" , ': Conventions = ', trim(dem%attributes%Conventions)
    if(len_trim(dem%attributes%title).gt.0) &
      print "(10x,2a)" , ': title = ', trim(dem%attributes%title)
    if(len_trim(dem%attributes%institution).gt.0) &
      print "(10x,2a)" , ': institution = ', trim(dem%attributes%institution)
    if(len_trim(dem%attributes%source).gt.0) &
      print "(10x,2a)" , ': source = ', trim(dem%attributes%source)
    if(len_trim(dem%attributes%history).gt.0) &
      print "(10x,2a)" , ': history = ', trim(dem%attributes%history)
    if(len_trim(dem%attributes%references).gt.0) &
      print "(10x,2a)" , ': references = ', trim(dem%attributes%references)
    if(len_trim(dem%attributes%comment).gt.0) &
      print "(10x,2a)" , ': comment = ', trim(dem%attributes%comment)
    print "(10x,a,l1)" , ': node_offset = ', dem%attributes%node_offset
!
    return
!
  End subroutine print_dem_info
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine get_dem_dims 
!
!       Get dimensions of a netCDF gridfile
!
!****************************
  Subroutine get_dem_dims ( ncid, dem )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer(int32), intent(in)     :: ncid
    type(type_dem), intent(inout)  :: dem
!
!.....Local variables
!
    integer(int32)          :: nDims, dimLen, id
    character(len=len_att)  :: dimName
!
!  ---
!
!.....Check number of dimensions
!
    call nc_check( nf90_inquire(ncid, nDimensions = nDims)  )
!
    if (nDims.ne.2) stop &
      'Error @ get_dem_dims: dimensions (lon/x and lat/y) expected in dem file!'
!
!.....examine dimensions
!
    do id=1,nDims
      call nc_check( nf90_inquire_dimension( ncid, dimid=id, name=dimName, len=dimLen ) )
      select case(strlowcase(dimName))
        case ('lon','x')
          dem%lon%var_name=dimName
          dem%lon%dimid=id
          dem%lon%count=dimLen
        case ('lat','y')
          dem%lat%var_name=dimName
          dem%lat%dimid=id
          dem%lat%count=dimLen
        case default
          stop 'Error @ get_dem_dims: only 2 dimensions (lon/lat or x/y) expected!'
      end select
    end do
!
    return
!
  End subroutine get_dem_dims
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine get_dem_vars 
!
!       Get variables of a netCDF gridfile
!
!****************************
  Subroutine get_dem_vars ( ncid, dem )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer(int32), intent(in)     :: ncid
    type(type_dem), intent(inout)  :: dem
!
!.....Local variables
!
    character(len=len_att)  :: varName
    integer(int32)          :: status, nVars, var, varDims
    integer(int32), dimension(nf90_max_var_dims)  :: varDimids
!
!  ---
!
!.....Check number of variables
!
    call nc_check( nf90_inquire(ncid, nVariables = nVars)  )
!
    if (nVars.lt.3) stop &
      'Error @ get_dem_data: not enough variables in dem file!'
!
!.....examine variables
!
    do var=1,nVars
      call nc_check( nf90_inquire_variable(ncid, varid=var, name=varName, ndims=varDims, dimids=varDimids) )
      select case (varDims)
        case(1)
          if (varDimids(1).eq.dem%lon%dimid) dem%lon%varid=var
          if (varDimids(1).eq.dem%lat%dimid) dem%lat%varid=var
        case(2)
          select case (strlowcase(varName))
            case('z','elev','elevation','dem')
              dem%elev%varid=var
              dem%elev%var_name=varName
              dem%elev%dimids=varDimids(1:varDims)
          end select
        case default
          stop 'Error @ get_dem_data: ony 2 dimensions expected!'
      end select
    end do
!
!.....get longitude attributes
!
    dem%lon%name='lon'
!
    call nc_check( nf90_get_att(ncid, dem%lon%varid, "units", dem%lon%units) )
    call nc_check( nf90_get_att(ncid, dem%lon%varid, "long_name", dem%lon%long_name) )
!
    status = nf90_get_att(ncid, dem%lon%varid, "axis", dem%lon%axis)
    if (status.ne.nf90_noerr) dem%lon%axis='X'
    status = nf90_get_att(ncid, dem%lon%varid, "standard_name", dem%lon%standard_name)
    if (status.ne.nf90_noerr) dem%lon%standard_name=dem%lon%long_name
!
!.....get latitude attributes 
!
    dem%lat%name="lat"
!
    call nc_check( nf90_get_att(ncid, dem%lat%varid, "units", dem%lat%units) )
    call nc_check( nf90_get_att(ncid, dem%lat%varid, "long_name", dem%lat%long_name) )
!
    status = nf90_get_att(ncid, dem%lat%varid, "axis", dem%lat%axis)
    if (status.ne.nf90_noerr) dem%lat%axis='Y'
    status = nf90_get_att(ncid, dem%lat%varid, "standard_name", dem%lat%standard_name)
    if (status.ne.nf90_noerr) dem%lat%standard_name=dem%lat%long_name
!
!.....get variable attributes
!
    dem%elev%name="elevation"
!
    status = nf90_get_att(ncid, dem%elev%varid, "long_name", dem%elev%long_name)
    if (status.ne.nf90_noerr) dem%elev%long_name='elevation'
!
    status = nf90_get_att(ncid, dem%elev%varid, "units", dem%elev%units)
    if (status.ne.nf90_noerr) dem%elev%units='m'
!
    status = nf90_get_att(ncid, dem%elev%varid, "standard_name", dem%elev%standard_name)
    if (status.ne.nf90_noerr) dem%elev%standard_name=dem%elev%long_name
!
    status = nf90_get_att(ncid, dem%elev%varid, "scale_factor", dem%elev%scale_factor)
    if (status.ne.nf90_noerr) dem%elev%scale_factor=1._single
!
    status = nf90_get_att(ncid, dem%elev%varid, "add_offset", dem%elev%add_offset)
    if (status.ne.nf90_noerr) dem%elev%add_offset=0._single
!
    status = nf90_get_att(ncid, dem%elev%varid, "_FillValue", dem%elev%FillValue)
    if (status.ne.nf90_noerr) dem%elev%FillValue=0._single
!
    status = nf90_get_att(ncid, dem%elev%varid, "valid_min", dem%elev%valid_min)
    status = nf90_get_att(ncid, dem%elev%varid, "valid_max", dem%elev%valid_max)
!
    return
!
  End subroutine get_dem_vars
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine get_dem_data 
!
!       Get dimensions of a netCDF gridfile
!
!****************************
  Subroutine get_dem_data ( ncid, dem )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer(int32), intent(in)     :: ncid
    type(type_dem), intent(inout)  :: dem
!
!  ---
!
!.....get lon and lat
!
    allocate( &
      dem%lon%dat( dem%lon%count ),  &
      dem%lat%dat( dem%lat%count ),  &
      dem%elev%dat( dem%lon%count, dem%lat%count ) &
    )
    call nc_check( nf90_get_var( ncid, dem%lon%varid, dem%lon%dat ) )
    call nc_check( nf90_get_var( ncid, dem%lat%varid, dem%lat%dat ) )
    call nc_check( nf90_get_var( ncid, dem%elev%varid, dem%elev%dat ) )
!
!.....Set info
!
    dem%lon%minval=dem%lon%dat(1)
    dem%lon%maxval=dem%lon%dat(dem%lon%count)
    dem%lon%range=dem%lon%maxval-dem%lon%minval
!
    dem%lat%minval=dem%lat%dat(1)
    dem%lat%maxval=dem%lat%dat(dem%lat%count)
    dem%lat%range=dem%lat%maxval-dem%lat%minval
!
    dem%lon%step=dms2deg(deg2dmsr(&
      sum(dem%lon%dat(2:dem%lon%count)-dem%lon%dat(1:dem%lon%count-1))/(dem%lon%count-1) &
      ))
    dem%lat%step=dms2deg(deg2dmsr(&
      sum(dem%lat%dat(2:dem%lat%count)-dem%lat%dat(1:dem%lat%count-1))/(dem%lat%count-1) &
      ))
!
    if (dem%attributes%node_offset) then
      dem%lon%offset=dem%lon%step/2
      dem%lat%offset=dem%lat%step/2
    else
      dem%lon%offset=0._double
      dem%lat%offset=0._double
    end if
!
    dem%lon%global = dem%lon%range.ge.360._double-dem%lon%step-2*dem%lon%offset
    dem%lat%global = dem%lat%range.ge.180._double-dem%lat%step-2*dem%lat%offset
!
    dem%elev%minval=minval(dem%elev%dat)
    dem%elev%maxval=maxval(dem%elev%dat)
    dem%elev%range=dem%elev%maxval-dem%elev%minval
!
    return
!
  End subroutine get_dem_data
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine blocksample_dem 
!
!       Resample dem grid 
!
!****************************
  Subroutine blocksample_dem ( dem, range_lon, range_lat, step, method, node_offset )
!****************************

    implicit none
!
!.....Dummy variables
!
    type(type_dem)            , intent(inout), target    :: dem
    real(double), dimension(2), intent(in)   , optional  :: range_lon, range_lat, step
    character(len=*)          , intent(in)   , optional  :: method
    logical                   , intent(in)   , optional  :: node_offset
!
!.....Local variables
!
    type(type_dem), target  :: newdem
!!      type(type_dms)  :: dem_step_dms
    character(len=len_att)     :: dum1, dum2, dum3, downsample_method
    character(len=len_att_gl)  :: history
    integer(int32)  :: i, j, ilon, ilat, nlon, nlat, rlon, rlat, block_lon, block_lat, block_cnt, blockSize
    real(single)    :: dummy, mu
    type(type_block_ptr), dimension(:,:), allocatable  :: blockGrid, blockEdge
    integer(int32)      , dimension(:)  , allocatable  :: ixlon1, ixlon2, exlon1, exlon2, ixlat1, ixlat2
    logical             , dimension(:)  , allocatable  :: exlon
!
!  ---
!
!.....Copy dem info
!
    call copy_dem_info(dem,newdem)
!
!.....Overwrite newdem settings with given arguments
!
    if (present(node_offset)) then
      newdem%attributes%node_offset=node_offset
    else
      newdem%attributes%node_offset=.false. ! force default node values instead of pixels
    end if
!
    if (present(range_lon)) then
      newdem%lon%minval=range_lon(1)
      newdem%lon%maxval=range_lon(2)
    end if
!
    if (present(range_lat)) then
      newdem%lat%minval=range_lat(1)
      newdem%lat%maxval=range_lat(2)
    end if
!
    if (present(step)) then
      newdem%lon%step=step(1)
      newdem%lat%step=step(2)
    end if
!
    if (present(method)) then
      select case (strlowcase(method))
        case ('blockmean','mean','mu')
          downsample_method='mean'
        case ('blocksd','sd','standard deviation','sigma') ! make additional output?
          downsample_method='sd'
        case ('blockse','standard error','se') ! make additional output?
          downsample_method='se'
        case ('blockci','confidence-interval','confidence interval','ci','95%') ! make additional output?
          downsample_method='ci'
        case ('blockmin','min')
          downsample_method='min'
        case ('blockmax','max')
          downsample_method='max'
        case ('blockrange','range') ! make additional output?
          downsample_method='range'
        case default
          stop 'Error @ blocksample dem : illegal downsample method provided (try: mean|sd|min|max|range)!'
      end select
    else
      downsample_method='mean'
    end if
!
!.....Check
!
    if(newdem%lon%minval.gt.newdem%lon%maxval) stop &
      'Error @ crop_dem : illegal longitude range provided!'
    if(newdem%lat%minval.gt.newdem%lat%maxval) stop &
      'Error @ crop_dem : illegal longitude range provided!'
    if(newdem%lon%step.lt.dem%lon%step) stop &
      'Error @ blockmean_dem : new longitude step is smaller than provide by dem. Only downsampling allowed!'
    if(newdem%lat%step.lt.dem%lat%step) stop &
      'Error @ blockmean_dem : new latitude step is smaller than provide by dem. Only downsampling allowed!'
!
!.....update newdem dimensions
!
    if (newdem%attributes%node_offset) then
      newdem%lon%offset=newdem%lon%step/2
      newdem%lat%offset=newdem%lat%step/2
    else
      newdem%lon%offset=0._double
      newdem%lat%offset=0._double
    end if
!
!.....Calculate full range (edge of pixels/nodes)
!
    newdem%lon%range=newdem%lon%maxval-newdem%lon%minval
    newdem%lat%range=newdem%lat%maxval-newdem%lat%minval
!
!.....Newdem global range?
!
    newdem%lon%global=newdem%lon%range.ge.360._double-newdem%lon%step+2*newdem%lon%offset
    newdem%lat%global=newdem%lat%range.ge.180._double-newdem%lat%step+2*newdem%lat%offset
!
    newdem%lon%minval=newdem%lon%minval+newdem%lon%offset
    if (newdem%lon%global) then
      newdem%lon%maxval=newdem%lon%minval+360._double-2*newdem%lon%offset
      newdem%lon%range=360._double-2*newdem%lon%offset
    else
      newdem%lon%maxval=newdem%lon%maxval-newdem%lon%offset
      newdem%lon%range= newdem%lon%range-2*newdem%lon%offset
    end if
    newdem%lon%count=nint(newdem%lon%range/newdem%lon%step+1_int32,kind=int32)
!
    if (newdem%lat%global) then
      newdem%lat%minval=-90._double+newdem%lat%offset
      newdem%lat%maxval=+90._double-newdem%lat%offset
      newdem%lat%range=180._double-2*newdem%lat%offset
    else
      newdem%lat%minval=newdem%lat%minval+newdem%lat%offset
      newdem%lat%maxval=newdem%lat%maxval-newdem%lat%offset
      newdem%lat%range= newdem%lat%range-2*newdem%lat%offset
    end if
    newdem%lat%count=nint(newdem%lat%range/newdem%lat%step+1_int32,kind=int32)
!
!.....Another check
!
    if (.not.dem%lon%global) then
      if (newdem%lon%range-2*newdem%lon%offset.gt.dem%lon%range+2*dem%lon%offset) stop &
        'Error @ blocksample_dem : new longitude range larger than provide by dem!'
      if (newdem%lon%minval-newdem%lon%offset.lt.dem%lon%minval-dem%lon%offset) stop &
        'Error @ blocksample_dem : longitude minval smallar than provide by dem!'
      if (newdem%lon%maxval+newdem%lon%offset.gt.dem%lon%maxval+dem%lon%offset) stop &
        'Error @ blocksample_dem : longitude maxval larger than provide by dem!'
    end if
!
    if (.not.dem%lat%global) then
      if (newdem%lat%range-2*newdem%lat%offset.gt.dem%lat%range+2*dem%lat%offset) stop &
        'Error @ blocksample_dem : new latitude range larger than provide by dem!'
      if (newdem%lat%minval-newdem%lat%offset.lt.dem%lat%minval-dem%lat%offset) stop &
        'Error @ blocksample_dem : latitude minval smallar than provide by dem!'
      if (newdem%lat%maxval+newdem%lat%offset.gt.dem%lat%maxval+dem%lat%offset) stop &
        'Error @ blocksample_dem : latitude maxval larger than provide by dem!'
    end if
!
!.....Allocate arrays
!
    allocate( &
      newdem%lon%dat( newdem%lon%count ),  &
      newdem%lat%dat( newdem%lat%count ),  &
      newdem%elev%dat( newdem%lon%count, newdem%lat%count ) &
    )
    newdem%lon%dat=(/ ( newdem%lon%minval+(i-1_int32)*newdem%lon%step, i=1_int32, newdem%lon%count ) /)
    newdem%lat%dat=(/ ( newdem%lat%minval+(i-1_int32)*newdem%lat%step, i=1_int32, newdem%lat%count ) /)
    newdem%elev%dat=0._single
!
!.....Block dimensions
!
    block_lon = nint(newdem%lon%step/dem%lon%step,kind=int32)
    block_lat = nint(newdem%lat%step/dem%lat%step,kind=int32)
    block_cnt = block_lon*block_lat
!
!.....Allocate block pointer structure
!
    allocate( &
      blockGrid(newdem%lon%count,newdem%lat%count), &
      blockEdge(newdem%lon%count,newdem%lat%count), &
      ixlon1(newdem%lon%count), ixlon2(newdem%lon%count), &
      ixlat1(newdem%lat%count), ixlat2(newdem%lat%count), &
      exlon1(newdem%lon%count), exlon2(newdem%lon%count), &
      exlon(newdem%lon%count) &
    )
    ixlon1=0_int32; ixlon2=0_int32;
    ixlat1=0_int32; ixlat2=0_int32;
    exlon1=0_int32; exlon2=0_int32;
    exlon=.false.
!
    nlon=dem%lon%count-block_lon+1_int32
    nlat=dem%lat%count-block_lat+1_int32
    rlon=floor(block_lon/2._double)
    rlat=floor(block_lat/2._double)
!
!! check if range is correct when given dem is not global! (fill with NaN??)
    do i=1_int32,newdem%lon%count
      ilon=nint((lonrangef(newdem%lon%dat(i),dem%lon%minval)-dem%lon%minval)/dem%lon%step+1_int32,kind=int32)
      ixlon1(i)=ilon-rlon
      ixlon2(i)=ilon+rlon
      if (ixlon1(i).lt.1_int32) then
        if (dem%lon%global) then
          exlon(i)=.true.
          exlon1(i)=ixlon1(i)+dem%lon%count
          exlon2(i)=dem%lon%count
        end if
        ixlon1(i)=1_int32
      elseif (ixlon2(i).gt.dem%lon%count) then
        if (dem%lon%global) then
          exlon(i)=.true.
          exlon1(i)=1_int32
          exlon2(i)=ixlon2(i)-dem%lon%count
        end if
        ixlon2(i)=dem%lon%count
      end if
    end do
    do j=1_int32,newdem%lat%count
      ilat=nint((newdem%lat%dat(j)-dem%lat%minval)/dem%lat%step+1_int32,kind=int32)
      ixlat1(j)=ilat-rlat
      if (ixlat1(j).lt.1_int32) ixlat1(j)=1_int32
      ixlat2(j)=ilat+rlat
      if (ixlat2(j).gt.dem%lat%count) ixlat2(j)=dem%lat%count
    end do
!
    do i=1_int32,newdem%lon%count
      do j=1_int32,newdem%lat%count
        blockGrid(i,j)%dat=>dem%elev%dat(ixlon1(i):ixlon2(i),ixlat1(j):ixlat2(j))
      end do
      if (exlon(i)) then
        do j=1_int32,newdem%lat%count
          blockEdge(i,j)%dat=>dem%elev%dat(exlon1(i):exlon2(i),ixlat1(j):ixlat2(j))
        end do
      end if
    end do
!
!.....Resample blocks
!
    select case (downsample_method)
      case ('mean')
        downsample_method='Block mean'
        do i=1_int32,newdem%lon%count
          if (exlon(i)) then
            do j=1_int32,newdem%lat%count
              blockSize=size(blockGrid(i,j)%dat)+size(blockEdge(i,j)%dat)
              newdem%elev%dat(i,j)=(sum(blockGrid(i,j)%dat)+sum(blockEdge(i,j)%dat))/blockSize
            end do
          else
            do j=1_int32,newdem%lat%count
              newdem%elev%dat(i,j)=sum(blockGrid(i,j)%dat)/size(blockGrid(i,j)%dat)
            end do
          end if
        end do
      case ('sd')
        downsample_method='Standard deviation of block mean'
        do i=1_int32,newdem%lon%count
          if (exlon(i)) then
            do j=1_int32,newdem%lat%count
              blockSize=size(blockGrid(i,j)%dat)+size(blockEdge(i,j)%dat)
              mu=(sum(blockGrid(i,j)%dat)+sum(blockEdge(i,j)%dat))/blockSize
              newdem%elev%dat(i,j)=sqrt(sum( (blockGrid(i,j)%dat-mu)**2 &
                + (blockEdge(i,j)%dat-mu)**2 )/blockSize)
            end do
          else
            do j=1_int32,newdem%lat%count
              blockSize=size(blockGrid(i,j)%dat)
              mu=sum(blockGrid(i,j)%dat)/blockSize
              newdem%elev%dat(i,j)=sqrt(sum((blockGrid(i,j)%dat-mu)**2)/blockSize)
            end do
          end if
        end do
      case ('se')
        downsample_method='Standard error of block mean'
        do i=1_int32,newdem%lon%count
          if (exlon(i)) then
            do j=1_int32,newdem%lat%count
              blockSize=size(blockGrid(i,j)%dat)+size(blockEdge(i,j)%dat)
              mu=(sum(blockGrid(i,j)%dat)+sum(blockEdge(i,j)%dat))/blockSize
              newdem%elev%dat(i,j)=sqrt(sum( (blockGrid(i,j)%dat-mu)**2 &
                + (blockEdge(i,j)%dat-mu)**2 )/blockSize**2)
            end do
          else
            do j=1_int32,newdem%lat%count
              blockSize=size(blockGrid(i,j)%dat)
              mu=sum(blockGrid(i,j)%dat)/blockSize
              newdem%elev%dat(i,j)=sqrt(sum((blockGrid(i,j)%dat-mu)**2)/blockSize**2)
            end do
          end if
        end do
      case ('ci')
        downsample_method='95% confidence interval of block mean'
        do i=1_int32,newdem%lon%count
          if (exlon(i)) then
            do j=1_int32,newdem%lat%count
              blockSize=size(blockGrid(i,j)%dat)+size(blockEdge(i,j)%dat)
              mu=(sum(blockGrid(i,j)%dat)+sum(blockEdge(i,j)%dat))/blockSize
              newdem%elev%dat(i,j)=1.96_single*sqrt(sum( (blockGrid(i,j)%dat-mu)**2 &
                + (blockEdge(i,j)%dat-mu)**2 )/blockSize**2)
            end do
          else
            do j=1_int32,newdem%lat%count
              blockSize=size(blockGrid(i,j)%dat)
              mu=sum(blockGrid(i,j)%dat)/blockSize
              newdem%elev%dat(i,j)=1.96_single*sqrt(sum((blockGrid(i,j)%dat-mu)**2)/blockSize**2)
            end do
          end if
        end do
      case ('min')
        downsample_method='Block minimum'
        do i=1_int32,newdem%lon%count
          if (exlon(i)) then
            do j=1_int32,newdem%lat%count
              newdem%elev%dat(i,j)=minval((/minval(blockGrid(i,j)%dat),minval(blockEdge(i,j)%dat)/))
            end do
          else
            do j=1_int32,newdem%lat%count
              newdem%elev%dat(i,j)=minval(blockGrid(i,j)%dat)
            end do
          end if
        end do
      case ('max')
        downsample_method='Block maximum'
        do i=1_int32,newdem%lon%count
          if (exlon(i)) then
            do j=1_int32,newdem%lat%count
              newdem%elev%dat(i,j)=maxval((/maxval(blockGrid(i,j)%dat),maxval(blockEdge(i,j)%dat)/))
            end do
          else
            do j=1_int32,newdem%lat%count
              newdem%elev%dat(i,j)=maxval(blockGrid(i,j)%dat)
            end do
          end if
        end do
      case ('range')
        downsample_method='Block range'
        do i=1_int32,newdem%lon%count
          if (exlon(i)) then
            do j=1_int32,newdem%lat%count
              dummy=minval((/minval(blockGrid(i,j)%dat),minval(blockEdge(i,j)%dat)/))
              newdem%elev%dat(i,j)=maxval((/maxval(blockGrid(i,j)%dat),maxval(blockEdge(i,j)%dat)/))-dummy
            end do
          else
            do j=1_int32,newdem%lat%count
              dummy=minval(blockGrid(i,j)%dat)
              newdem%elev%dat(i,j)=maxval(blockGrid(i,j)%dat)-dummy
            end do
          end if
        end do
    end select
    newdem%elev%long_name=trim(downsample_method)//' '//strlowcase(trim(newdem%elev%long_name))
!
!.....Deallocate block pointer
!
    deallocate(blockGrid,blockEdge,ixlon1,ixlon2,ixlat1,ixlat2,exlon1,exlon2,exlon)
!
!.....Set elevation specs
!
    newdem%elev%minval=minval(newdem%elev%dat)
    newdem%elev%maxval=maxval(newdem%elev%dat)
    newdem%elev%range=newdem%elev%maxval-newdem%elev%minval
!
!.....extend history
!
    history = 'Downsampled elevation using blocks. Nodes/pixels correspond to ' &
      // strlowcase(trim(downsample_method)) // '. New elevation map ranges and step intervals: '
!
    write( dum1, "(f12.7)" ) newdem%lon%minval 
    i=scan(dum1(6:10),'0',.false.); if (i.gt.0_int32)dum1=dum1(1:4+i)
    write( dum2, "(f12.7)" ) newdem%lon%maxval
    i=scan(dum2(6:10),'0',.false.); if (i.gt.0_int32)dum2=dum2(1:4+i)
    write( dum3, "(f12.7)" ) newdem%lon%step
    i=scan(dum3(6:10),'0',.false.); if (i.gt.0_int32)dum3=dum3(1:4+i)
    write( history, "(a,' longitude [',a,':',a,':',a,']')" ) trim(history), &
      trim(adjustl(dum1)), trim(adjustl(dum2)), trim(adjustl(dum3))
!
    write( dum1, "(f12.7)" ) newdem%lat%minval
    i=scan(dum1(6:10),'0',.false.); if (i.gt.0_int32)dum1=dum1(1:4+i)
    write( dum2, "(f12.7)" ) newdem%lat%maxval
    i=scan(dum2(6:10),'0',.false.); if (i.gt.0_int32)dum2=dum2(1:4+i)
    write( dum3, "(f12.7)" ) newdem%lat%step
    i=scan(dum3(6:10),'0',.false.); if (i.gt.0_int32)dum3=dum3(1:4+i)
    write( history, "(a,' latitude [',a,':',a,':',a,']')" ) trim(history), &
      trim(adjustl(dum1)), trim(adjustl(dum2)), trim(adjustl(dum3))
!
    newdem%attributes%history=trim(history)//'. '//trim(newdem%attributes%history)
!
!.....Copy output
!
    dem=newdem
    call deallocate_dem(newdem)
!
    return
!
  End subroutine blocksample_dem
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine copy_Dem 
!
!       Duplicate of a netCDF gridfile
!
!****************************
  Subroutine copy_dem ( dem, newdem )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_dem)              , intent(in)   :: dem
    type(type_dem)              , intent(out)  :: newdem
!
!  ---
!
!.....copy header
!
    call copy_dem_info(dem,newdem)
!
!.....copy data
!
    if (allocated(dem%lon%dat)) then
      if (allocated(newdem%lon%dat)) deallocate(newdem%lon%dat)
      allocate(newdem%lon%dat(newdem%lon%count))
      newdem%lon%dat=dem%lon%dat
    end if
    if (allocated(dem%lat%dat)) then
      if (allocated(newdem%lat%dat)) deallocate(newdem%lat%dat)
      allocate(newdem%lat%dat(newdem%lat%count))
      newdem%lat%dat=dem%lat%dat
    end if
    allocate(newdem%elev%dat(newdem%lon%count,newdem%lat%count))
    newdem%elev%dat=dem%elev%dat
!
    return
!
  End subroutine copy_dem
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine get_dem_data 
!
!       Get dimensions of a netCDF gridfile
!
!****************************
  Subroutine copy_dem_info ( dem, newdem )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_dem)              , intent(in)   :: dem
    type(type_dem)              , intent(out)  :: newdem
!
!  ---
!
!.....copy
!
    newdem%file=dem%file
    newdem%ncid=dem%ncid
!
    call copy_dem_dim_info ( dem%lon, newdem%lon )
    call copy_dem_dim_info ( dem%lat, newdem%lat )
    call copy_dem_var_info ( dem%elev, newdem%elev )
    call copy_dem_att_info ( dem%attributes, newdem%attributes )
!
    return
!
  End subroutine copy_dem_info
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine copy_dem_dim_info 
!
!       Copy dem dimension info
!
!****************************
  Subroutine copy_dem_dim_info ( dim1, dim2 )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_dem_dim)          , intent(in)   :: dim1
    type(type_dem_dim)          , intent(out)  :: dim2
!
!  ---
!
    dim2%dimid=dim1%dimid
    dim2%varid=dim1%varid
    dim2%count=dim1%count
!
    dim2%name=dim1%name
    dim2%long_name=dim1%long_name
    dim2%standard_name=dim1%standard_name
    dim2%units=dim1%units
    dim2%var_name=dim1%var_name
    dim2%axis=dim1%axis
!
    dim2%minval=dim1%minval
    dim2%maxval=dim1%maxval
    dim2%range=dim1%range
    dim2%step=dim1%step
!
    return
!
  End subroutine copy_dem_dim_info
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine copy_dem_var_info 
!
!       Copy dem variable info
!
!****************************
  Subroutine copy_dem_var_info ( var1, var2 )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_dem_var)          , intent(in)   :: var1
    type(type_dem_var)          , intent(out)  :: var2
!
!  ---
!
    var2%dimids=var1%dimids
    var2%varid=var1%varid
!
    var2%name=var1%name
    var2%long_name=var1%long_name
    var2%standard_name=var1%standard_name
    var2%units=var1%units
    var2%var_name=var1%var_name
!
    var2%scale_factor=var1%scale_factor
    var2%add_offset=var1%add_offset
    var2%missing_value=var1%missing_value
    var2%fillValue=var1%fillValue
    var2%valid_min=var1%valid_min
    var2%valid_max=var1%valid_max
    var2%minval=var1%minval
    var2%maxval=var1%maxval
    var2%range=var1%range
!
    return
!
  End subroutine copy_dem_var_info
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine copy_dem_att_info 
!
!       Copy dem attribute info
!
!****************************
  Subroutine copy_dem_att_info ( att1, att2 )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_dem_att)          , intent(in)   :: att1
    type(type_dem_att)          , intent(out)  :: att2
!
!  ---
!
    att2%Conventions=att1%Conventions
    att2%title=att1%title
    att2%institution=att1%institution
    att2%source=att1%source
    att2%history=att1%history
    att2%references=att1%references
    att2%comment=att1%comment
!
    att2%node_offset=att1%node_offset
!
    return
!
  End subroutine copy_dem_att_info
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine clear_dem
!
!       Clear dem type 
!
!****************************
  Subroutine clear_dem ( dem )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_dem), intent(inout)  :: dem
!
!  ---
!
    dem%file=''
    dem%ncid=0_int32
    call clear_dem_dim(dem%lon)
    call clear_dem_dim(dem%lat)
    call clear_dem_var(dem%elev)
    call clear_dem_att(dem%attributes)
!
    return
!
  End subroutine clear_dem
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine clear_dem_dim
!
!       Clear dem dimension type
!
!****************************
  Subroutine clear_dem_dim ( demDim )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_dem_dim), intent(inout)  :: demDim
!
!  ---
!
    demDim%dimid=0_int32
    demDim%varid=0_int32
    demDim%count=0_int32
!
    demDim%name=''
    demDim%long_name=''
    demDim%standard_name=''
    demDim%units=''
    demDim%var_name=''
    demDim%axis=''
!
    demDim%step=0._double
    demDim%minval=0._double
    demDim%maxval=0._double
    demDim%range=0._double
!
    if (allocated(demDim%dat)) deallocate(demDim%dat)
!
    return
!
  End subroutine clear_dem_dim
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine clear_dem_var
!
!       Clear dem variable type
!
!****************************
  Subroutine clear_dem_var ( demVar )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_dem_var), intent(inout)  :: demVar
!
!  ---
!
    demVar%varid=0_int32
    demVar%dimids=0_int32
!
    demVar%name=''
    demVar%long_name=''
    demVar%standard_name=''
    demVar%units=''
    demVar%var_name=''
!
    demVar%scale_factor=1._double
    demVar%add_offset=0._double
    demVar%missing_value=0._double
    demVar%fillValue=0._double
    demVar%valid_min=0._double
    demVar%valid_max=0._double
    demVar%minval=0._double
    demVar%maxval=0._double
    demVar%range=0._double
!
    if (allocated(demVar%dat)) deallocate(demVar%dat)
!
    return
!
  End subroutine clear_dem_var
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine clear_dem_att
!
!       Clear dem attribute type
!
!****************************
  Subroutine clear_dem_att ( demAtt )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_dem_att), intent(inout)  :: demAtt
!
!  ---
!
    demAtt%Conventions=''
    demAtt%title=''
    demAtt%institution=''
    demAtt%source=''
    demAtt%history=''
    demAtt%references=''
    demAtt%comment=''
!
    demAtt%node_offset=.false.
!
    return
!
  End subroutine clear_dem_att
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine deallocate_dem
!
!       Deallocate dem arrays
!
!****************************
  Subroutine deallocate_dem ( dem )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    type(type_dem), intent(inout)  :: dem
!
!  ---
!
    if (allocated(dem%lon%dat)) deallocate(dem%lon%dat)
    if (allocated(dem%lat%dat)) deallocate(dem%lat%dat)
    if (allocated(dem%elev%dat)) deallocate(dem%elev%dat)
!
    return
!
  End subroutine deallocate_dem
!
! =========================================================================================

! =========================================================================================
!
!.......Subroutine get_dem_globals 
!
!       Get global information in netCDF gridfile
!
!****************************
  Subroutine get_dem_global_attributes ( ncid, dem )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer(int32), intent(in)     :: ncid
    type(type_dem), intent(inout)  :: dem
!
!.....Local variables
!
    character(len=len_att_gl)  :: attName
    integer(int32)             :: nGlobalAtts, att, dummy
!
!  ---
!
!.....Check number of variables
!
    call nc_check( nf90_inquire(ncid, nAttributes = nGlobalAtts)  )
!
!.....examine attributes 
!
    do att=1_int32,nGlobalAtts
      call nc_check( nf90_inq_attname(ncid, varid=nf90_global, attnum=att, name=attName) )
      select case( strlowcase(attName) )
        case('conventions')
          call get_dem_attribute_value( ncid, nf90_global, attName, dem%attributes%conventions )
        case('title')
          call get_dem_attribute_value( ncid, nf90_global, attName, dem%attributes%title )
        case('institution')
          call get_dem_attribute_value( ncid, nf90_global, attName, dem%attributes%institution )
        case('source')
          call get_dem_attribute_value( ncid, nf90_global, attName, dem%attributes%source )
        case('history')
          call get_dem_attribute_value( ncid, nf90_global, attName, dem%attributes%history )
        case('references')
          call get_dem_attribute_value( ncid, nf90_global, attName, dem%attributes%references )
        case('comment')
          call get_dem_attribute_value( ncid, nf90_global, attName, dem%attributes%comment )
        case('node_offset')
          call nc_check( nf90_get_att( ncid, nf90_global, attName, dummy) )
          dem%attributes%node_offset = dummy.eq.1
        case default
      end select
    end do
!
    return
!
  End subroutine get_dem_global_attributes
!
! =========================================================================================


! =========================================================================================
!
!.......Subroutine get_dem_globals 
!
!       Get global information in netCDF gridfile
!
!****************************
  Subroutine get_dem_attribute_value ( ncid, varid, attName, value )
!****************************
!
    implicit none
!
!.....Dummy variables
!
    integer(int32)       , intent(in )  :: ncid, varid
    character(len=*)     , intent(in )  :: attName
    character(len=*)     , intent(out)  :: value
!
!.....Local variables
!
    integer(int32)          :: attLen
!
!  ---
!
!.....examine attributes 
!
    call nc_check( nf90_inquire_attribute(ncid, varid, name=attName, len=attLen) )
!
    if (len(value).lt.attLen) then
      print "(3a)", "Warning: not enough space to put attribute value for ", trim(attName), '.'
    else
      call nc_check( nf90_get_att(ncid, varid, attName, value) )
    end if
!
    return
!
  End subroutine get_dem_attribute_value
!
! =========================================================================================

End module libdem
