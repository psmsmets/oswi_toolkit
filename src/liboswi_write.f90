Subroutine oswi_write_nc ( file, oswi, compression, verbose, verboseAll )
!*****************************************************************************80
!
!! OSWI_WRITE_NC 
!
!  Description:
!
!    Write oswi data to netcdf4 file
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
!    ...
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  character(len=*), intent(in)            :: file
  type(type_oswi) , intent(inout)         :: oswi
  logical         , intent(in), optional  :: verbose, compression, verboseAll
!
! Local variables
!
  logical         :: verb, verbAll, comp
  integer(int32)  :: ncid
!
! Init
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  verball=.false.
  if (present(verboseAll)) verball=verboseAll
  comp=.true.
  if (present(compression)) comp=compression
!
  if (verb) write(output_unit,"('> ',2a)") "Write oswi data to NetCDF4 file ", file
!
! Create the netCDF file.
!
  call oswi_create_nc( file, ncid=ncid, verbose=verbAll )
!
! ...Define the groups
!
    call oswi_nc_def_grp ( ncid, oswi, verbAll )
!
! ...Define the dimensions
!
    call oswi_nc_def_dim( ncid, oswi, verbAll )
!
! ...Define the variables
!
    call oswi_nc_def_var( ncid, oswi, verbAll )
!
! ...Define variables deflation
!
    if (comp) call oswi_nc_def_var_deflate( ncid, oswi, verbAll )
!
! ...Add the variable attributes
!
    call oswi_netcdf_put_att( ncid, oswi, verbAll )
!
! ...Add the global attributes
!
    call oswi_netcdf_put_global( ncid, oswi, verbAll  )
!
! End definitions
!
  call oswi_nc_enddef( ncid=ncid, verbose=verbAll ) 
!
! ...Put the data
!
    call oswi_nc_put_var( ncid, oswi, verbAll )
!
! Close the netCDF file
!
  if (verbAll) write(output_unit,"(a)") "  > Close netcdf file"
  call nc_check( nf90_close(ncid), verbose=verbAll )
!
  return
!
!*****************************************************************************80
End subroutine oswi_write_nc


Subroutine oswi_create_nc ( file, ncid, verbose )
!*****************************************************************************80
!
!! OSWI_CREATE_NC 
!
!  Description:
!
!    Create new netcdf4 file
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
!    ...
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  character(len=*), intent(in )           :: file
  integer(int32)  , intent(out)           :: ncid
  logical        , intent(in ), optional  :: verbose
!
! Local variables
!
  logical        :: verb
!
! ----
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write (output_unit,"(a)") "  > Create netcdf file (classic, NetCDF4)"
!
  call nc_check( nf90_create( path=file, cmode=NF90_NETCDF4, ncid=ncid ), verbose=verb )
!
  return
!
!*****************************************************************************80
End subroutine oswi_create_nc


Subroutine oswi_nc_enddef ( ncid, verbose )
!*****************************************************************************80
!
!! OSWI_NC_ENDDEF
!
!  Description:
!
!    ...
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
!    ...
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  integer(int32), intent(in)           :: ncid
  logical       , intent(in), optional  :: verbose
!
! Local variables
!
  logical  :: verb
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write (output_unit,"(a)") "  > Close netcdf header definition"
!
  call nc_check( nf90_enddef(ncid), verbose=verb )
!
  return
!
!*****************************************************************************80
End subroutine oswi_nc_enddef


Subroutine oswi_nc_def_grp ( ncid, oswi, verbose )
!*****************************************************************************80
!
!! OSWI_NC_DEF_GRP
!
!  Description:
!
!    ...
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
!    ...
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  integer(int32) , intent(in)            :: ncid
  type(type_oswi), intent(in)            :: oswi
  logical        , intent(in), optional  :: verbose
!
! Local variables
!
  logical        :: verb
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write (output_unit,"(a)") "  >> Create netcdf groups and subgroups"
!
! Create groups and subgroups
!
  wg_env = -1_int32
  if (.not.oswi%hdr%swh.and..not.oswi%hdr%swi.and..not.oswi%hdr%type.eq.'dem') &
    call nc_check( nf90_def_grp( ncid, "environment", wg_env ), verbose=verb )
!
  return
!
!*****************************************************************************80
End subroutine oswi_nc_def_grp


Subroutine oswi_nc_def_dim ( ncid, oswi, verbose )
!*****************************************************************************80
!
!! OSWI_NC_DEF_DIM
!
!  Description:
!
!    ...
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
!    ...
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  integer(int32) , intent(in)            :: ncid
  type(type_oswi), intent(in)            :: oswi
  logical        , intent(in), optional  :: verbose
!
! Local variables
!
  logical        :: verb
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write (output_unit,"(a)") "  >> Define netcdf dimensions"
!
! Define max string dimensions
!
  call nc_check( nf90_def_dim( ncid, s_strLen, StrLen, d_strLen ), verbose=verb )
!
! Define data dimensions
!
  if (oswi%hdr%grid%distinctGridValues) then
    call nc_check( nf90_def_dim( ncid, nc_dim_lon, oswi%hdr%grid%lon%count, wd_lon ), verbose=verb )
    call nc_check( nf90_def_dim( ncid, nc_dim_lat, oswi%hdr%grid%lat%count, wd_lat ), verbose=verb )
  elseif (oswi%hdr%grid%gridType.eq.'reduced_ll') then
    call nc_check( nf90_def_dim( ncid, nc_dim_points, oswi%hdr%grid%numberOfPoints, wd_nofp ), verbose=verb )
    call nc_check( nf90_def_dim( ncid, nc_dim_lat, oswi%hdr%grid%lat%size, wd_lat ), verbose=verb )
  elseif (oswi%hdr%grid%gridType.eq.'icosahedron') then
    call nc_check( nf90_def_dim( ncid, nc_dim_points, oswi%hdr%grid%numberOfPoints, wd_nofp ), verbose=verb )
  end if
  if (.not.oswi%hdr%integrate) call nc_check( nf90_def_dim( ncid, nc_dim_freq, oswi%hdr%f%count, wd_freq ), verbose=verb )
!
  return
!
!*****************************************************************************80
End subroutine oswi_nc_def_dim


Subroutine oswi_nc_def_var ( ncid, oswi, verbose )
!*****************************************************************************80
!
!! OSWI_NC_DEF_VAR
!
!  Description:
!
!    ...
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
!    ...
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  integer(int32) , intent(in)            :: ncid
  type(type_oswi), intent(in)            :: oswi
  logical        , intent(in), optional  :: verbose
!
! Local variables
!
  logical        :: verb
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
!
  if (verb) write (output_unit,"(a)") "  >> Define netcdf variables"
!
  call nc_check( nf90_def_var( ncid, s_time, nf90_int64 , v_time ) )
  call nc_check( nf90_def_var( ncid, s_tstr, nf90_char  , d_strLen, v_tstr ) )
  call nc_check( nf90_def_var( ncid, s_tmat, nf90_double, v_tmat ) )
!
  if (oswi%hdr%grid%distinctGridValues) then
    call nc_check( nf90_def_var( ncid, nc_var_lon, nf90_double, wd_lon, wv_lon ) )
    call nc_check( nf90_def_var( ncid, nc_var_lat, nf90_double, wd_lat, wv_lat ) )
    if (oswi%hdr%integrate) then
      call nc_check( nf90_def_var( ncid, nc_var_freq, nf90_byte, wv_freq ) )
      call nc_check( nf90_def_var( ncid, trim(oswi%hdr%name), nf90_float, (/wd_lon,wd_lat/), wv_oswi ) )
      if (oswi%hdr%fanalysis) then
        call nc_check( nf90_def_var( ncid, nc_var_freqd, nf90_float, (/wd_lon,wd_lat/), wv_fd ) )
        call nc_check( nf90_def_var( ncid, nc_var_freqc, nf90_float, (/wd_lon,wd_lat/), wv_fc ) )
        call nc_check( nf90_def_var( ncid, nc_var_freqrms, nf90_float, (/wd_lon,wd_lat/), wv_frms ) )
        call nc_check( nf90_def_var( ncid, nc_var_freqbw, nf90_float, (/wd_lon,wd_lat/), wv_fbw ) )
      end if
    else
      call nc_check( nf90_def_var( ncid, nc_var_freq, nf90_double, wd_freq, wv_freq ) )
      call nc_check( nf90_def_var( ncid, trim(oswi%hdr%name), nf90_float, (/wd_lon,wd_lat,wd_freq/), wv_oswi ) )
    end if
  else
    call nc_check( nf90_def_var( ncid, nc_var_lon, nf90_double, wd_nofp, wv_lon ) )
    if (oswi%hdr%grid%gridType.eq.'reduced_ll') then
      call nc_check( nf90_def_var( ncid, nc_var_lat, nf90_double, wd_lat , wv_lat ) )
      call nc_check( nf90_def_var( ncid, nc_var_pl , nf90_short , wd_lat , wv_pl  ) )
    elseif (oswi%hdr%grid%gridType.eq.'icosahedron') then
      call nc_check( nf90_def_var( ncid, nc_var_lat, nf90_double, wd_nofp , wv_lat ) )
    end if
    if (oswi%hdr%integrate) then
      call nc_check( nf90_def_var( ncid, nc_var_freq, nf90_byte, wv_freq ) )
      call nc_check( nf90_def_var( ncid, trim(oswi%hdr%name), nf90_float, (/wd_nofp/), wv_oswi ) )
      if (oswi%hdr%fanalysis) then
        call nc_check( nf90_def_var( ncid, nc_var_freqd, nf90_float, (/wd_nofp/), wv_fd ) )
        call nc_check( nf90_def_var( ncid, nc_var_freqc, nf90_float, (/wd_nofp/), wv_fc ) )
        call nc_check( nf90_def_var( ncid, nc_var_freqrms, nf90_float, (/wd_nofp/), wv_frms ) )
        call nc_check( nf90_def_var( ncid, nc_var_freqbw, nf90_float, (/wd_nofp/), wv_fbw ) )
      end if
    else
      call nc_check( nf90_def_var( ncid, nc_var_freq, nf90_double, wd_freq, wv_freq ) )
      call nc_check( nf90_def_var( ncid, trim(oswi%hdr%name), nf90_float, (/wd_nofp,wd_freq/), wv_oswi ) )
    end if
  end if
!
  call nc_check( nf90_def_var( ncid, nc_var_grid, nf90_byte, wv_grid ) )
  call nc_check( nf90_def_var( ncid, nc_var_wave, nf90_byte, wv_wave ) )
  if (oswi%hdr%dem%defined) call nc_check( nf90_def_var( ncid, nc_var_dem, nf90_byte, wv_dem ) )
!
! Environment
!
  if (wg_env.ne.-1_int32) then
    call nc_check( nf90_def_var( wg_env, trim(oswi%hdr%env%rho_air%name), nf90_double, wv_rho_air ) )
    call nc_check( nf90_def_var( wg_env, trim(oswi%hdr%env%rho_sea%name), nf90_double, wv_rho_sea ) )
    call nc_check( nf90_def_var( wg_env, trim(oswi%hdr%env%rho_bed%name), nf90_double, wv_rho_bed ) )
    call nc_check( nf90_def_var( wg_env, trim(oswi%hdr%env%c_air%name), nf90_double, wv_c_air ) )
    call nc_check( nf90_def_var( wg_env, trim(oswi%hdr%env%c_sea%name), nf90_double, wv_c_sea ) )
    call nc_check( nf90_def_var( wg_env, trim(oswi%hdr%env%c_bed%name), nf90_double, wv_c_bed ) )
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_nc_def_var


Subroutine oswi_netcdf_put_global ( ncid, oswi, verbose )
!*****************************************************************************80
!
!! OSWI_NC_PUT_GLOBAL
!
!  Description:
!
!    ...
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
!    ...
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  integer(int32) , intent(in)            :: ncid
  type(type_oswi), intent(in)            :: oswi
  logical        , intent(in), optional  :: verbose
!
! Local variables
!
  logical        :: verb
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
!
  if (verb) write (output_unit,"(a)") "  >> Put netcdf global attributes"
!
! Global attributes 
!
  call nc_check( nf90_put_att( ncid, nf90_global, nc_att_featureType, &
    'test' ) )
  call nc_check( nf90_put_att( ncid, nf90_global, nc_att_Conventions, &
    'CF-1.6' ) )
  call nc_check( nf90_put_att( ncid, nf90_global, nc_att_title, &
    'filename' ) )
  call nc_check( nf90_put_att( ncid, nf90_global, nc_att_institution, &
    'Royal Netherlands Meteorological Institute (KNMI)' ) )
  call nc_check( nf90_put_att( ncid, nf90_global, nc_att_source, &
    'Royal Netherlands Meteorological Institute (KNMI)' ) )
  call nc_check( nf90_put_att( ncid, nf90_global, nc_att_comment, &
    '' ) )
  call nc_check( nf90_put_att( ncid, nf90_global, nc_att_created, timestamp() ) )
  call nc_check( nf90_put_att( ncid, nf90_global, nc_att_version, trim(liboswi_version()) ) )
  call nc_check( nf90_put_att( ncid, nf90_global, nc_att_history, trim(oswi%hdr%history) ) )
!
  return
!
!*****************************************************************************80
End subroutine oswi_netcdf_put_global


Subroutine oswi_netcdf_put_att ( ncid, oswi, verbose )
!*****************************************************************************80
!
!! OSWI_NC_PUT_ATT
!
!  Description:
!
!    ...
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
!    ...
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  integer(int32) , intent(in)            :: ncid
  type(type_oswi), intent(in), target    :: oswi
  logical        , intent(in), optional  :: verbose
!
! Local variables
!
  logical                                :: verb
  real(double), pointer, dimension(:)    :: f
  character(len=nc_strLen)               :: f_long_name, f_standard_name
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
!
  if (verb) write (output_unit,"(a)") "  >> Put netcdf variable attributes"
!
! Set frequency
!
  if (oswi%hdr%swh.or.oswi%hdr%swi) then
    f => oswi%hdr%f%wave
    f_long_name     = 'Sea surface wave'
    f_standard_name = 'Sea_surface_wave'
  else
    f => oswi%hdr%f%sound
    f_long_name     = 'Sound'
    f_standard_name = 'Sound'
  endif
!
!..time
!
  call nc_check( nf90_put_att( ncid, v_time, nc_att_long_name , trim(l_time) ) )
  call nc_check( nf90_put_att( ncid, v_time, nc_att_units     , trim(u_time) ) )
!
  call nc_check( nf90_put_att( ncid, v_tstr, nc_att_long_name , trim(l_tstr) ) )
  call nc_check( nf90_put_att( ncid, v_tstr, nc_att_units     , trim(u_tstr) ) )
!
  call nc_check( nf90_put_att( ncid, v_tmat, nc_att_long_name , trim(l_tmat) ) )
  call nc_check( nf90_put_att( ncid, v_tmat, nc_att_units     , trim(u_tmat) ) )
!
!..grid
!
  call nc_check( nf90_put_att( ncid, wv_lon, nc_att_long_name    , 'longitude' ) )
  call nc_check( nf90_put_att( ncid, wv_lon, nc_att_standard_name, 'longitude' ) )
  call nc_check( nf90_put_att( ncid, wv_lon, nc_att_units        , 'degrees_east' ) )
  call nc_check( nf90_put_att( ncid, wv_lon, nc_att_axis        , 'X' ) )
  if (oswi%hdr%grid%lon%reversed) then
    call nc_check( nf90_put_att( ncid, wv_lon, nc_att_valid_min, oswi%hdr%grid%lon%last ) )
    call nc_check( nf90_put_att( ncid, wv_lon, nc_att_valid_max, oswi%hdr%grid%lon%first ) )
  else
    call nc_check( nf90_put_att( ncid, wv_lon, nc_att_valid_min, oswi%hdr%grid%lon%first ) )
    call nc_check( nf90_put_att( ncid, wv_lon, nc_att_valid_max, oswi%hdr%grid%lon%last ) )
  end if
!
  call nc_check( nf90_put_att( ncid, wv_lat, nc_att_long_name    , 'latitude' ) )
  call nc_check( nf90_put_att( ncid, wv_lat, nc_att_standard_name, 'latitude' ) )
  call nc_check( nf90_put_att( ncid, wv_lat, nc_att_units        , 'degrees_north' ) )
  call nc_check( nf90_put_att( ncid, wv_lat, nc_att_axis        , 'Y' ) )
  if (oswi%hdr%grid%lat%reversed) then
    call nc_check( nf90_put_att( ncid, wv_lat, nc_att_valid_min, oswi%hdr%grid%lat%last ) )
    call nc_check( nf90_put_att( ncid, wv_lat, nc_att_valid_max, oswi%hdr%grid%lat%first ) )
  else
    call nc_check( nf90_put_att( ncid, wv_lat, nc_att_valid_min, oswi%hdr%grid%lat%first ) )
    call nc_check( nf90_put_att( ncid, wv_lat, nc_att_valid_max, oswi%hdr%grid%lat%last ) )
  end if
  if (allocated(oswi%hdr%grid%lat%mask)) then
    call nc_check( nf90_put_att( ncid, wv_lat, nc_att_min_value, &
      minval(oswi%hdr%grid%lat%dat,mask=oswi%hdr%grid%lat%mask) ) )
    call nc_check( nf90_put_att( ncid, wv_lat, nc_att_max_value, &
      maxval(oswi%hdr%grid%lat%dat,mask=oswi%hdr%grid%lat%mask) ) )
  else
    call nc_check( nf90_put_att( ncid, wv_lat, nc_att_min_value, &
      minval(oswi%hdr%grid%lat%dat) ) )
    call nc_check( nf90_put_att( ncid, wv_lat, nc_att_max_value, &
      maxval(oswi%hdr%grid%lat%dat) ) )
  end if
!
  if (oswi%hdr%grid%gridType.eq.'reduced_ll') then
    call nc_check( nf90_put_att( ncid, wv_pl, nc_att_long_name, 'number of points along a full parallel' ) )
    call nc_check( nf90_put_att( ncid, wv_pl, nc_att_units    , '-' ) )
    call nc_check( nf90_put_att( ncid, wv_pl, nc_att_valid_min, 0_int16 ) )
    call nc_check( nf90_put_att( ncid, wv_pl, nc_att_valid_max, int(maxval(oswi%hdr%grid%pl),int16) ) )
  end if
!
  call nc_check( nf90_put_att( ncid, wv_grid, nc_att_gridType, trim(oswi%hdr%grid%gridType) ) )
  if (     oswi%hdr%grid%earthIsOblate) &
    & call nc_check( nf90_put_att( ncid, wv_grid, nc_att_earthIsOblate, nf90_true ) )
  if (.not.oswi%hdr%grid%earthIsOblate) &
    & call nc_check( nf90_put_att( ncid, wv_grid, nc_att_earthIsOblate, nf90_false ) )
  if (     oswi%hdr%grid%interpolated) call nc_check( nf90_put_att(ncid,wv_grid,nc_att_interpolated,nf90_true ) )
  if (.not.oswi%hdr%grid%interpolated) call nc_check( nf90_put_att(ncid,wv_grid,nc_att_interpolated,nf90_false) )
  call nc_check( nf90_put_att( ncid, wv_grid, nc_att_earthRadius, oswi%hdr%grid%earthRadius ) )
  if (oswi%hdr%grid%gridType.eq.'icosahedron') &
    call nc_check( nf90_put_att( ncid, wv_grid, nc_att_icosahedronEdge, oswi%hdr%grid%icosahedronEdge ) )
!
! frequency
!
  call nc_check( nf90_put_att( ncid, wv_freq, nc_att_long_name    , trim(f_long_name)//' frequency' ) )
  call nc_check( nf90_put_att( ncid, wv_freq, nc_att_standard_name, trim(f_standard_name)//'_frequency' ) )
  call nc_check( nf90_put_att( ncid, wv_freq, nc_att_units        , 's-1' ) )
  call nc_check( nf90_put_att( ncid, wv_freq, nc_att_min_value    , f(oswi%hdr%f%ixFirst) ) )
  call nc_check( nf90_put_att( ncid, wv_freq, nc_att_max_value    , f(oswi%hdr%f%ixLast) ) )
  if (.not.oswi%hdr%integrate) then
    call nc_check( nf90_put_att( ncid, wv_freq, nc_att_axis         , 'F' ) )
    call nc_check( nf90_put_att( ncid, wv_freq, nc_att_integrated   , nf90_false ) )
  else
    call nc_check( nf90_put_att( ncid, wv_freq, nc_att_numberOfValues, oswi%hdr%f%count ) )
    call nc_check( nf90_put_att( ncid, wv_freq, nc_att_integrated   , nf90_true ) )
!
    if (oswi%hdr%fanalysis) then
!
      call nc_check( nf90_put_att( ncid, wv_fc, nc_att_long_name      , trim(f_long_name)//' center frequency' ) )
      call nc_check( nf90_put_att( ncid, wv_fc, nc_att_units          , 's-1' ) )
      call nc_check( nf90_put_att( ncid, wv_fc, nc_att_missing_value  , real(oswi%hdr%missingValue,kind=single) ) )
      call nc_check( nf90_put_att( ncid, wv_fc, nc_att_valid_min      , f(oswi%hdr%f%ixFirst) ) )
      call nc_check( nf90_put_att( ncid, wv_fc, nc_att_valid_max      , f(oswi%hdr%f%ixLast) ) )
!
      call nc_check( nf90_put_att( ncid, wv_fd, nc_att_long_name      , trim(f_long_name)//' dominant frequency' ) )
      call nc_check( nf90_put_att( ncid, wv_fd, nc_att_units          , 's-1' ) )
      call nc_check( nf90_put_att( ncid, wv_fd, nc_att_missing_value  , real(oswi%hdr%missingValue,kind=single) ) )
      call nc_check( nf90_put_att( ncid, wv_fd, nc_att_valid_min      , 0._single ) )
      call nc_check( nf90_put_att( ncid, wv_fd, nc_att_valid_min      , f(oswi%hdr%f%ixFirst) ) )
      call nc_check( nf90_put_att( ncid, wv_fd, nc_att_valid_max      , f(oswi%hdr%f%ixLast) ) )
!
      call nc_check( nf90_put_att( ncid, wv_fbw, nc_att_long_name     , trim(f_long_name)//' spectral bandwidth' ) )
      call nc_check( nf90_put_att( ncid, wv_fbw, nc_att_units         , 's-1' ) )
      call nc_check( nf90_put_att( ncid, wv_fbw, nc_att_missing_value , real(oswi%hdr%missingValue,kind=single) ) )
      call nc_check( nf90_put_att( ncid, wv_fbw, nc_att_valid_min     , 0._single ) )
      call nc_check( nf90_put_att( ncid, wv_fbw, nc_att_valid_max     , f(oswi%hdr%f%ixLast) ) )
!
      call nc_check( nf90_put_att( ncid, wv_frms, nc_att_long_name    , trim(f_long_name)//' rms frequency' ) )
      call nc_check( nf90_put_att( ncid, wv_frms, nc_att_units        , 's-1' ) )
      call nc_check( nf90_put_att( ncid, wv_frms, nc_att_missing_value, real(oswi%hdr%missingValue,kind=single) ) )
      call nc_check( nf90_put_att( ncid, wv_frms, nc_att_valid_min    , f(oswi%hdr%f%ixFirst) ) )
      call nc_check( nf90_put_att( ncid, wv_frms, nc_att_valid_max    , f(oswi%hdr%f%ixLast) ) )
!
    end if
!
  end if
!
!..oswi
!
  call nc_check( nf90_put_att( ncid, wv_oswi, nc_att_long_name    , trim(oswi%hdr%long_name) ) )
  call nc_check( nf90_put_att( ncid, wv_oswi, nc_att_standard_name, trim(oswi%hdr%standard_name) ) )
  call nc_check( nf90_put_att( ncid, wv_oswi, nc_att_units        , trim(oswi%hdr%units) ) )
  call nc_check( nf90_put_att( ncid, wv_oswi, nc_att_missing_value, real(oswi%hdr%missingValue,kind=single) ) )
  if (oswi%hdr%integrate) then
    call nc_check( nf90_put_att( ncid, wv_oswi, nc_att_max_value, oswi%dat%ll%maxValue ) )
  else
    call nc_check( nf90_put_att( ncid, wv_oswi, nc_att_max_value, oswi%dat%llf%maxValue ) )
  end if
  if (oswi%hdr%dB) then
    call nc_check( nf90_put_att( ncid, wv_oswi, nc_att_valid_min, -300._double ) )
    call nc_check( nf90_put_att( ncid, wv_oswi, nc_att_valid_max,  300._double ) )
  elseif (oswi%hdr%lg) then
    call nc_check( nf90_put_att( ncid, wv_oswi, nc_att_valid_min, -15._double ) )
    call nc_check( nf90_put_att( ncid, wv_oswi, nc_att_valid_max,   9._double ) )
  else
    call nc_check( nf90_put_att( ncid, wv_oswi, nc_att_valid_min,   0._double ) )
    call nc_check( nf90_put_att( ncid, wv_oswi, nc_att_valid_max,   1.d9      ) )
  end if
!
! osw
!
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_short_name   , trim(oswi%hdr%osw%shortName) ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_long_name    , 'Sea surface wave spectra input' ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_units        , trim(oswi%hdr%osw%units) ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_centre       , trim(oswi%hdr%osw%centre) ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_dataClass    , trim(oswi%hdr%osw%dataClass) ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_dataType     , trim(oswi%hdr%osw%dataType) ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_dataStream   , trim(oswi%hdr%osw%dataStream) ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_gridType     , trim(oswi%hdr%osw%gridType) ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_versionNumber, oswi%hdr%osw%experimentVersionNumber ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_editionNumber, oswi%hdr%osw%editionNumber ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_paramId      , oswi%hdr%osw%paramId ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_frequencies  , oswi%hdr%osw%frequencies ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_firstFrequency, oswi%hdr%osw%firstFrequency ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_frequencyScalar, oswi%hdr%osw%frequencyScalar ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_directions   , oswi%hdr%osw%directions ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_directions   , oswi%hdr%osw%directions ) )
  call nc_check( nf90_put_att( ncid, wv_wave, nc_att_directionIncr, oswi%hdr%osw%directionIncrement ) )
  if (     oswi%hdr%osw%hasselmann) call nc_check( nf90_put_att( ncid, wv_wave, nc_att_Hasselmann, nf90_true ) )
  if (.not.oswi%hdr%osw%hasselmann) call nc_check( nf90_put_att( ncid, wv_wave, nc_att_Hasselmann, nf90_false ) )
  if (     oswi%hdr%osw%directionFullCircle) &
    call nc_check( nf90_put_att( ncid, wv_wave, nc_att_directionFull, nf90_true ) )
  if (.not.oswi%hdr%osw%directionFullCircle) &
    call nc_check( nf90_put_att( ncid, wv_wave, nc_att_directionFull, nf90_false ) )
  if (     oswi%hdr%osw%interpolated) &
    call nc_check( nf90_put_att( ncid, wv_wave, nc_att_interpolated, nf90_true ) )
  if (.not.oswi%hdr%osw%interpolated) &
    call nc_check( nf90_put_att( ncid, wv_wave, nc_att_interpolated, nf90_false ) )
!
! depth information
!
  if (oswi%hdr%dem%defined) then
    if (     oswi%hdr%dem%node_offset)  call nc_check( nf90_put_att(ncid,wv_dem,nc_att_node_offset,nf90_true ) )
    if (.not.oswi%hdr%dem%node_offset)  call nc_check( nf90_put_att(ncid,wv_dem,nc_att_node_offset,nf90_false) )
    if (     oswi%hdr%dem%interpolated) call nc_check( nf90_put_att(ncid,wv_dem,nc_att_interpolated,nf90_true ) )
    if (.not.oswi%hdr%dem%interpolated) call nc_check( nf90_put_att(ncid,wv_dem,nc_att_interpolated,nf90_false) )
    call nc_check( nf90_put_att( ncid, wv_dem, nc_att_long_name      , 'DEM bathymetry input (sea floor depth)' ) )
    call nc_check( nf90_put_att( ncid, wv_dem, nc_att_units          , trim(oswi%hdr%dem%units) ) )
    call nc_check( nf90_put_att( ncid, wv_dem, nc_att_resample_method, trim(oswi%hdr%dem%resampleMethod) ) )
    call nc_check( nf90_put_att( ncid, wv_dem, nc_att_title          , trim(oswi%hdr%dem%title) ) )
    call nc_check( nf90_put_att( ncid, wv_dem, nc_att_institution    , trim(oswi%hdr%dem%institution) ) )
    call nc_check( nf90_put_att( ncid, wv_dem, nc_att_source         , trim(oswi%hdr%dem%source) ) )
    call nc_check( nf90_put_att( ncid, wv_dem, nc_att_history        , trim(oswi%hdr%dem%history) ) )
    call nc_check( nf90_put_att( ncid, wv_dem, nc_att_references     , trim(oswi%hdr%dem%references) ) )
  end if
!
! Enviroment
!
  if (wg_env.ne.-1_int32) then
!
    call nc_check( nf90_put_att( wg_env, wv_rho_air, nc_att_long_name, trim(oswi%hdr%env%rho_air%long_name) ) )
    call nc_check( nf90_put_att( wg_env, wv_rho_air, nc_att_standard_name, trim(oswi%hdr%env%rho_air%standard_name) ) )
    call nc_check( nf90_put_att( wg_env, wv_rho_air, nc_att_units, trim(oswi%hdr%env%rho_air%units) ) )
!
    call nc_check( nf90_put_att( wg_env, wv_rho_sea, nc_att_long_name, trim(oswi%hdr%env%rho_sea%long_name) ) )
    call nc_check( nf90_put_att( wg_env, wv_rho_sea, nc_att_standard_name, trim(oswi%hdr%env%rho_sea%standard_name) ) )
    call nc_check( nf90_put_att( wg_env, wv_rho_sea, nc_att_units, trim(oswi%hdr%env%rho_sea%units) ) )
!
    call nc_check( nf90_put_att( wg_env, wv_rho_bed, nc_att_long_name, trim(oswi%hdr%env%rho_bed%long_name) ) )
    call nc_check( nf90_put_att( wg_env, wv_rho_bed, nc_att_standard_name, trim(oswi%hdr%env%rho_bed%standard_name) ) )
    call nc_check( nf90_put_att( wg_env, wv_rho_bed, nc_att_units, trim(oswi%hdr%env%rho_bed%units) ) )
!
    call nc_check( nf90_put_att( wg_env, wv_c_air, nc_att_long_name, trim(oswi%hdr%env%c_air%long_name) ) )
    call nc_check( nf90_put_att( wg_env, wv_c_air, nc_att_standard_name, trim(oswi%hdr%env%c_air%standard_name) ) )
    call nc_check( nf90_put_att( wg_env, wv_c_air, nc_att_units, trim(oswi%hdr%env%c_air%units) ) )
!
    call nc_check( nf90_put_att( wg_env, wv_c_sea, nc_att_long_name, trim(oswi%hdr%env%c_sea%long_name) ) )
    call nc_check( nf90_put_att( wg_env, wv_c_sea, nc_att_standard_name, trim(oswi%hdr%env%c_sea%standard_name) ) )
    call nc_check( nf90_put_att( wg_env, wv_c_sea, nc_att_units, trim(oswi%hdr%env%c_sea%units) ) )
!
    call nc_check( nf90_put_att( wg_env, wv_c_bed, nc_att_long_name, trim(oswi%hdr%env%c_bed%long_name) ) )
    call nc_check( nf90_put_att( wg_env, wv_c_bed, nc_att_standard_name, trim(oswi%hdr%env%c_bed%standard_name) ) )
    call nc_check( nf90_put_att( wg_env, wv_c_bed, nc_att_units, trim(oswi%hdr%env%c_bed%units) ) )
!
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_netcdf_put_att


Subroutine oswi_nc_def_var_deflate ( ncid, oswi, verbose )
!*****************************************************************************80
!
!! OSWI_NC_DEF_VAR_DEFLATE
!
!  Description:
!
!    ...
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
!    ...
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  integer(int32) , intent(in)            :: ncid
  type(type_oswi), intent(in)            :: oswi
  logical        , intent(in), optional  :: verbose
!
! Local variables
!
  logical        :: verb
  integer(int32), parameter  :: shuffle = 1_int32 ! If non-zero, turn on the shuffle filter.
  integer(int32), parameter  :: deflate = 1_int32 ! If non-zero, turn on deflate specified by level.
  integer(int32), parameter  :: level   = 9_int32 ! Set deflate level [1,9]
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write (output_unit,"(a)") "  >> Define netcdf variable deflation"
!
! Define large variable deflation
!
  call nc_check( nf90_def_var_deflate ( ncid, wv_oswi, shuffle, deflate, level ) )
  call nc_check( nf90_def_var_deflate ( ncid, wv_lon, shuffle, deflate, level ) )
  call nc_check( nf90_def_var_deflate ( ncid, wv_lat, shuffle, deflate, level ) )
  if (oswi%hdr%grid%gridType.eq.'reduced_ll') &
    call nc_check( nf90_def_var_deflate ( ncid, wv_pl, shuffle, deflate, level ) )
  if (.not.oswi%hdr%integrate) &
    call nc_check( nf90_def_var_deflate ( ncid, wv_freq, shuffle, deflate, level ) )
  if (oswi%hdr%fanalysis) then
    call nc_check( nf90_def_var_deflate ( ncid, wv_fc, shuffle, deflate, level ) )
    call nc_check( nf90_def_var_deflate ( ncid, wv_fd, shuffle, deflate, level ) )
    call nc_check( nf90_def_var_deflate ( ncid, wv_fbw, shuffle, deflate, level ) )
    call nc_check( nf90_def_var_deflate ( ncid, wv_frms, shuffle, deflate, level ) )
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_nc_def_var_deflate


Subroutine oswi_nc_put_var ( ncid, oswi, verbose )
!*****************************************************************************80
!
!! OSWI_NC_PUT_VAR
!
!  Description:
!
!    ...
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
!    ...
!
!
!****************************
!
  use time, only: epoch2str
  use time, only: epoch2julday
  implicit none
!
! Dummy variables
!
  integer(int32) , intent(in)            :: ncid
  type(type_oswi), intent(in)            :: oswi
  logical        , intent(in), optional  :: verbose
!
! Local variables
!
  character       :: tstr*26
  logical        :: verb
  real(double), dimension(:,:)  , pointer      :: regular_ll
  real(double), dimension(:,:,:), pointer      :: regular_llf
!
! ----
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
!
  if (verb) write (output_unit,"(a)") "  >> Put netcdf variable data"
!
!..time
!
  if (verb) write (output_unit,"(a)") "   >>> Time"
  call nc_check( nf90_put_var( ncid, v_time, oswi%dat%epoch ) )
  call epoch2str( oswi%dat%epoch, tstr )
  call nc_check( nf90_put_var( ncid, v_tstr, trim(tstr) ) )
  call nc_check( nf90_put_var( ncid, v_tmat, epoch2julday(oswi%dat%epoch) ) )
!
! OWSI
!
  if (oswi%hdr%grid%distinctGridValues) then
    if (verb) write (output_unit,"(a)") "   >>> Distinct grid values"
    if (allocated(oswi%hdr%grid%lat%mask)) then
      call nc_check( nf90_put_var( ncid, wv_lat, pack(oswi%hdr%grid%lat%dat,oswi%hdr%grid%lat%mask) ) )
    else
      call nc_check( nf90_put_var( ncid, wv_lat, oswi%hdr%grid%lat%dat ) )
    end if
    if (allocated(oswi%hdr%grid%lon%mask)) then
      call nc_check( nf90_put_var( ncid, wv_lon, pack(oswi%hdr%grid%lon%dat,oswi%hdr%grid%lon%mask) ) )
    else
      call nc_check( nf90_put_var( ncid, wv_lon, oswi%hdr%grid%lon%dat ) )
    end if
    if (oswi%hdr%integrate) then
      regular_ll => oswi_write_reshape_ll( oswi%dat%ll%dat, &
        (/oswi%hdr%grid%lon%count,oswi%hdr%grid%lat%count/) )
      call nc_check( nf90_put_var( ncid, wv_oswi, real(regular_ll,single) ) )
      if (oswi%hdr%fanalysis) then
        regular_ll => oswi_write_reshape_ll( oswi%dat%ll%fc, &
          (/oswi%hdr%grid%lon%count,oswi%hdr%grid%lat%count/) )
        call nc_check( nf90_put_var( ncid, wv_fc, real(regular_ll,single) ) )
        regular_ll => oswi_write_reshape_ll( oswi%dat%ll%fd, &
          (/oswi%hdr%grid%lon%count,oswi%hdr%grid%lat%count/) )
        call nc_check( nf90_put_var( ncid, wv_fd, real(regular_ll,single) ) )
        regular_ll => oswi_write_reshape_ll( oswi%dat%ll%fb, &
          (/oswi%hdr%grid%lon%count,oswi%hdr%grid%lat%count/) )
        call nc_check( nf90_put_var( ncid, wv_fbw, real(regular_ll,single) ) )
        regular_ll => oswi_write_reshape_ll( oswi%dat%ll%fr, &
          (/oswi%hdr%grid%lon%count,oswi%hdr%grid%lat%count/) )
        call nc_check( nf90_put_var( ncid, wv_frms, real(regular_ll,single) ) )
      end if
    else
      regular_llf => oswi_write_reshape_llf( oswi%dat%llf%dat, &
        (/oswi%hdr%grid%lon%count,oswi%hdr%grid%lat%count,oswi%hdr%f%count/) )
      call nc_check( nf90_put_var( ncid, wv_oswi, real(regular_llf,single) ) )
    end if
  else
    call nc_check( nf90_put_var( ncid, wv_lon, oswi%hdr%grid%lon%dat ) )
    call nc_check( nf90_put_var( ncid, wv_lat, oswi%hdr%grid%lat%dat ) )
    if (oswi%hdr%grid%gridType.eq.'reduced_ll') then
      if (verb) write (output_unit,"(a)") "   >>> Reduced_ll grid values"
      call nc_check( nf90_put_var( ncid, wv_pl, int(oswi%hdr%grid%pl,int16) ) )
    else
      if (verb) write (output_unit,"(a)") "   >>> Icosahedron grid values"
    end if
    if (oswi%hdr%integrate) then
      call nc_check( nf90_put_var( ncid, wv_oswi, real(oswi%dat%ll%dat,single) ) )
      if (oswi%hdr%fanalysis) then
        call nc_check( nf90_put_var( ncid, wv_fc, real(oswi%dat%ll%fc,single) ) )
        call nc_check( nf90_put_var( ncid, wv_fd, real(oswi%dat%ll%fd,single) ) )
        call nc_check( nf90_put_var( ncid, wv_fbw, real(oswi%dat%ll%fb,single) ) )
        call nc_check( nf90_put_var( ncid, wv_frms, real(oswi%dat%ll%fr,single) ) )
      end if
    else
      call nc_check( nf90_put_var( ncid, wv_oswi, real(oswi%dat%llf%dat,single) ) )
    end if
  end if
!
! Frequency
!
  if (verb) write (output_unit,"(a)") "   >>> Frequency"
  if (.not.oswi%hdr%integrate) then
    if (oswi%hdr%swh.or.oswi%hdr%swi) then
      call nc_check( nf90_put_var( ncid, wv_freq, oswi%hdr%f%wave ) )
    else
      call nc_check( nf90_put_var( ncid, wv_freq, oswi%hdr%f%sound ) )
    end if
  end if
!
! General stuff
!
  if (verb) write (output_unit,"(a)") "   >>> Wave spectra info"
  call nc_check( nf90_put_var( ncid, wv_wave, nf90_true ) )
  call nc_check( nf90_put_var( ncid, wv_grid, nf90_true ) )
  if (oswi%hdr%dem%defined) call nc_check( nf90_put_var( ncid, wv_dem, nf90_true ) )
!
! Environment
!
  if (wg_env.ne.-1_int32) then
    if (verb) write (output_unit,"(a)") "   >>> Environment variables"
    call nc_check( nf90_put_var( wg_env, wv_rho_air, oswi%hdr%env%rho_air%value ) )
    call nc_check( nf90_put_var( wg_env, wv_rho_sea, oswi%hdr%env%rho_sea%value ) )
    call nc_check( nf90_put_var( wg_env, wv_rho_bed, oswi%hdr%env%rho_bed%value ) )
    call nc_check( nf90_put_var( wg_env, wv_c_air, oswi%hdr%env%c_air%value ) )
    call nc_check( nf90_put_var( wg_env, wv_c_sea, oswi%hdr%env%c_sea%value ) )
    call nc_check( nf90_put_var( wg_env, wv_c_bed, oswi%hdr%env%c_bed%value ) )
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_nc_put_var


Function oswi_write_reshape_ll(array, shape_) result(aptr)
!*****************************************************************************80
!
!! OSWI_WRITE_RESHAPE_LL
!
!  Description:
!
!    ...
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
!    ...
!
!
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
!
! Use C_LOC to get the start location of the array data, and
! use C_F_POINTER to turn this into a fortran pointer (aptr).
! Note that we need to specify the shape of the pointer using an
! integer array.
  call C_F_POINTER(C_LOC(array), aptr, shape_)
!
!*****************************************************************************80
End function oswi_write_reshape_ll


Function oswi_write_reshape_llf(array, shape_) result(aptr)
!*****************************************************************************80
!
!! OSWI_WRITE_RESHAPE_LL
!
!  Description:
!
!    ...
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
!    ...
!
!
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
!
! Use C_LOC to get the start location of the array data, and
! use C_F_POINTER to turn this into a fortran pointer (aptr).
! Note that we need to specify the shape of the pointer using an
! integer array.
  call C_F_POINTER(C_LOC(array), aptr, shape_)
!
!*****************************************************************************80
End function oswi_write_reshape_llf
