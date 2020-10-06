Subroutine oswi_read ( nc, oswi, verbose, debug, header_only, mask )
!*****************************************************************************80
!
!! OSWI_READ
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
  character(len=*), intent(in)             :: nc
  type(type_oswi) , intent(inout)          :: oswi
  logical         , intent(in) , optional  :: verbose, debug, header_only
  logical         , intent(out), optional  :: mask
!
! Local variables
!
  logical         :: verb, dbug, halt, get_data 
  integer(int32)  :: ncid
  character(len=len_att)  :: pre='Error @ oswi_read_nc :'
!
! Init
!
  if (present(mask)) then 
    halt=.false.
    mask=.false.
  else
    halt=.true.
  end if
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  dbug=.false.
  if (present(debug)) dbug=debug
  if (dbug) verb=.true.
  get_data=.true.
  if (present(header_only)) get_data=.not.header_only
!
! Open and read in
!
  if (verb) write(output_unit,"('> ',2a)") 'Read oswi NetCDF file ', trim(nc)
!
! Open the netCDF file.
  if (dbug) write(output_unit,"(2x,'>> ',a)") 'Open NetCDF file'
  call nc_check( &
    nf90_open( nc, nf90_nowrite, ncid ), &
    verbose=dbug, mask=mask, prefix=pre &
    )
  if (.not.halt.and..not.mask) return
! Get the groups
  call oswi_read_nc_inq_grp ( ncid=ncid, hdr=oswi%hdr, verbose=dbug, mask=mask )
  if (.not.halt.and..not.mask) return
! Get the dimensions
  call oswi_read_nc_get_dim ( ncid=ncid, hdr=oswi%hdr, verbose=dbug, mask=mask )
  if (.not.halt.and..not.mask) return
! Inquire the variables and attributes
  call oswi_read_nc_inq_var( ncid=ncid, hdr=oswi%hdr, verbose=dbug, mask=mask )
  if (.not.halt.and..not.mask) return
! Get header information
  call oswi_read_nc_get_hdr( ncid=ncid, hdr=oswi%hdr, verbose=dbug )
  if (.not.halt.and..not.mask) return
! Get the data
  if (get_data) then
    call liboswi_allocate_type_dat( oswi%hdr, oswi%dat )
    call oswi_read_dat ( ncid=ncid, hdr=oswi%hdr, dat=oswi%dat, verbose=dbug, mask=mask )
    if (.not.halt.and..not.mask) return
  end if
! Close the netCDF file
  if (dbug) write(output_unit,"(2x,'>> ',a)") 'Close netcdf file'
  call nc_check( nf90_close(ncid), verbose=dbug, mask=mask, prefix=pre )
  if (.not.halt.and..not.mask) return
!
  return
!
!*****************************************************************************80
End subroutine oswi_read


Subroutine oswi_read_hdr ( nc, hdr, verbose, debug, mask )
!*****************************************************************************80
!
!! OSWI_READ_NC_HDR
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
  character(len=*)   , intent(in)             :: nc
  type(type_oswi_hdr), intent(inout)          :: hdr
  logical            , intent(in) , optional  :: verbose, debug
  logical            , intent(out), optional  :: mask
!
! Local variables
!
  logical         :: verb, dbug, halt 
  integer(int32)  :: ncid
  character(len=len_att)  :: pre='Error @ oswi_read_hdr :'
!
! Init
!
  if (present(mask)) then 
    halt=.false.
    mask=.false.
  else
    halt=.true.
  end if
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  dbug=.false.
  if (present(debug)) dbug=debug
  if (dbug) verb=.true.
!
! Open and read in
!
  if (verb) write(output_unit,"('> ',2a)") 'Read oswi NetCDF file ', trim(nc)
!
! Open the netCDF file.
  if (dbug) write(output_unit,"(2x,'>> ',a)") 'Open NetCDF file'
  call nc_check( &
    nf90_open( nc, nf90_nowrite, ncid ), &
    verbose=dbug, mask=mask, prefix=pre &
    )
  if (.not.halt.and..not.mask) return
! Get the groups
  call oswi_read_nc_inq_grp ( ncid=ncid, hdr=hdr, verbose=dbug, mask=mask )
  if (.not.halt.and..not.mask) return
! Get the dimensions
  call oswi_read_nc_get_dim ( ncid=ncid, hdr=hdr, verbose=dbug, mask=mask )
  if (.not.halt.and..not.mask) return
! Inquire the variables and attributes
  call oswi_read_nc_inq_var( ncid=ncid, hdr=hdr, verbose=dbug, mask=mask )
  if (.not.halt.and..not.mask) return
! Get header information
  call oswi_read_nc_get_hdr( ncid=ncid, hdr=hdr, verbose=dbug )
  if (.not.halt.and..not.mask) return
! Close the netCDF file
  if (dbug) write(output_unit,"(2x,'>> ',a)") 'Close netcdf file'
  call nc_check( nf90_close(ncid), verbose=dbug, mask=mask, prefix=pre )
  if (.not.halt.and..not.mask) return
!
  return
!
!*****************************************************************************80
End subroutine oswi_read_hdr


Subroutine oswi_read_nc_epoch ( nc, epoch, timestring, mask, verbose )
!*****************************************************************************80
!
!! OSWI_READ_NC_EPOCH
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
  character(len=*), intent(in)             :: nc
  integer(int64)  , intent(out)            :: epoch
  character(len=*), intent(out), optional  :: timestring
  logical         , intent(out), optional  :: mask
  logical         , intent(in) , optional  :: verbose
!
! Local variables
!
  logical                 :: halt, verb
  character(len=len_att)  :: varName, pre='Error @ oswi_read_nc_epoch :'
  integer(int32)          :: ncid, var, nvars
!
! ----
!
! Init
!
  if (present(mask)) then 
    halt=.false.
    mask=.false.
  else
    halt=.true.
  end if
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write(output_unit,"('> ',2a)") 'Get epoch from oswi NetCDF file ', trim(nc)
!
  epoch = 0_int64
  if (present(timestring)) timestring=''
!
! Get epoch data
!
  v_time=-1_int32
! Open the netCDF file.
  call nc_check( &
    nf90_open( nc, nf90_nowrite, ncid ), &
    verbose=verb, mask=mask, prefix=pre &
    ); if (.not.halt.and..not.mask) return
! get number of variables
  call nc_check( &
    nf90_inquire(ncid, nVariables = nVars), &
    verbose=verb, mask=mask, prefix=pre &
    ); if (.not.halt.and..not.mask) return
! examine variables
  do var=1_int32,nVars
    call nc_check( &
      nf90_inquire_variable(ncid, varid=var, name=varName), &
      verbose=verb, mask=mask, prefix=pre &
      )
    if (.not.halt.and..not.mask) return
    if (trim(varName).eq.'epoch') then
      v_time = var
      exit
    end if
  end do
  if (v_time.eq.-1_int32) then
    if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'no epoch variable found!'
    if (halt) stop; mask=.false.; return
  end if
! get epoch from file
  call nc_check( &
    nf90_get_var( ncid, v_time, epoch ), &
    verbose=verb, mask=mask, prefix=pre &
    ); if (.not.halt.and..not.mask) return
! Close the netCDF file
  call nc_check( nf90_close(ncid), verbose=verb, mask=mask, prefix=pre )
  if (.not.halt.and..not.mask) return
!
! wrap up
!
  if (present(timestring)) call epoch2str( epoch, timestring )
  if (.not.halt) mask=.true.
  return
!
!*****************************************************************************80
End subroutine oswi_read_nc_epoch


Subroutine oswi_read_nc_data ( nc, dat, verbose, debug, mask )
!*****************************************************************************80
!
!! OSWI_READ_NC_DATA
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
  character(len=*)            , intent(in)             :: nc
  type(type_oswi_dat)         , intent(inout)          :: dat
  logical                     , intent(in) , optional  :: verbose, debug
  logical                     , intent(out), optional  :: mask
!
! Local variables
!
  type(type_oswi_hdr)     :: hdr
  logical                 :: verb, dbug, halt
  integer(int32)          :: ncid
  character(len=len_att)  :: pre='Error @ oswi_read_nc_data :'
!
! Init
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  dbug=.false.
  if (present(debug)) dbug=debug
  if (dbug) verb=.true.
  if (verb) write(output_unit,"('> ',2a)") 'Get oswi data from NetCDF file ', trim(nc)
!
  if (present(mask)) then 
    halt=.false.
    mask=.false.
  else
    halt=.true.
  end if     
!
! Open the netCDF file.
!
  if (dbug) write(output_unit,"(2x,'>> ',a)") 'Open NetCDF file'
  call nc_check( &
    nf90_open( nc, nf90_nowrite, ncid ), &
    verbose=dbug, mask=mask, prefix=pre &
    )
  if (.not.halt.and..not.mask) return
!
! Get basic stuff (no header vectors!)
!
! Get the dimensions
  call oswi_read_nc_get_dim ( ncid=ncid, hdr=hdr, verbose=dbug, mask=mask )
  if (.not.halt.and..not.mask) return
! Inquire the variables and attributes
  call oswi_read_nc_inq_var( ncid=ncid, hdr=hdr, verbose=dbug, mask=mask )
  if (.not.halt.and..not.mask) return
!
! Get the data
!
  if (hdr%integrate) then
    call oswi_read_dat_ll ( ncid, hdr, dat%ll, dbug, mask )
  else
    call oswi_read_dat_llf ( ncid, hdr, dat%llf, dbug, mask )
  end if
!
! Close the netCDF file
!
  if (dbug) write(output_unit,"(2x,'>> ',a)") 'Close netcdf file'
  call nc_check( nf90_close(ncid), verbose=dbug, mask=mask, prefix=pre )
  if (.not.halt.and..not.mask) return

!
  if (.not.halt) mask=.true.
  return
!
!*****************************************************************************80
End subroutine oswi_read_nc_data


Function oswi_compare_nc_with_type ( nc, hdr, relax ) result( match )
!*****************************************************************************80
!
!! OSWI_COMPARE_NC_WITH_TYPE
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
  character(len=*)   , intent(in)            :: nc
  type(type_oswi_hdr), intent(in)            :: hdr
  logical            , intent(in), optional  :: relax
  logical                                    :: match
!
! Local variables
!
  type(type_oswi_hdr)  :: dummy
!
  call oswi_read_hdr ( nc=trim(nc), hdr=dummy, mask=match )

  if (.not.match) return
  match = oswi_compare_hdr( hdr, dummy, relax )
!
  return
!
!*****************************************************************************80
End function oswi_compare_nc_with_type


Subroutine oswi_read_nc_inq_grp ( ncid, hdr, verbose, mask )
!*****************************************************************************80
!
!! OSWI_READ_NC_INQ_GRP
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
  integer(int32)     , intent(in)             :: ncid
  type(type_oswi_hdr), intent(inout)          :: hdr
  logical            , intent(in) , optional  :: verbose
  logical            , intent(out), optional  :: mask
!
! Local variables
!
  logical                 :: verb, halt, ok
  integer(int32)          :: nGrps, grp
  character(len=len_att)  :: grpName, pre='Error @ oswi_read_nc_inq_grp :'
  integer(int32), dimension(nf90_max_var_dims)  :: grpIds
!
! Init
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write(output_unit,"(2x,'>> ',a)") 'Inquire netcdf groups and subgroups'
!
  if (present(mask))then
    halt=.false.
    mask=.false.
  else
    halt=.true.
  endif
!
! Init
!
  rg_env=-1_int32
!
! Inquire groups
!
  call nc_check( &
    nf90_inq_grps(ncid, numgrps=nGrps, ncids=grpIds), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
!
! examine groups
!
  do grp=1_int32,nGrps
    call nc_check( &
      nf90_inq_grpname( ncid=grpIds(grp), name=grpName ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    select case(grpName)
      case ('environment')
        rg_env=grpIds(grp)
    end select
  end do
!
  if (.not.halt)mask=.true.
  return
!
!*****************************************************************************80
End subroutine oswi_read_nc_inq_grp


Subroutine oswi_read_nc_get_dim ( ncid, hdr, verbose, mask )
!*****************************************************************************80
!
!! OSWI_READ_NC_GET_DIM
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
!  implicit none
!
! Dummy variables
!
  integer(int32)     , intent(in)             :: ncid
  type(type_oswi_hdr), intent(inout)          :: hdr
  logical            , intent(in) , optional  :: verbose
  logical            , intent(out), optional  :: mask
!
! Local variables
!
  logical                 :: verb, halt, ok
  integer(int32)          :: nDims, dimLen, id
  character(len=len_att)  :: dimName, pre='Error @ oswi_read_nc_get_dim :'
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write(output_unit,"(2x,'>> ',a)") 'Get oswi netcdf dimensions'
!
  if (.not.halt)then
    halt=.false.
    mask=.false.
  else
    halt=.true.
  endif
!
! Init
!
  call oswi_init_hdr( hdr, clear=.true., dealloc=.false. )
!
  hdr%grid%numberOfPoints=-1_int32
  hdr%grid%lat%size=-1_int32
  hdr%grid%lon%size=-1_int32
  hdr%f%size=1_int32
  hdr%integrate=.true.
  hdr%grid%distinctGridValues=.true.
!
! Check number of dimensions
!
  call nc_check( &
    nf90_inquire(ncid, nDimensions = nDims), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    )
  if (.not.ok) return
!
! examine dimensions
!
  do id=1_int32,nDims
    call nc_check( &
      nf90_inquire_dimension( ncid, dimid=id, name=dimName, len=dimLen ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    select case(dimName)
      case (nc_dim_points)
        hdr%grid%distinctGridValues=.false.
        hdr%grid%numberOfPoints=dimLen
        rd_nofp=id
      case (nc_dim_lat)
        hdr%grid%lat%size=dimLen
        rd_lat=id
      case (nc_dim_lon)
        hdr%grid%distinctGridValues=.true.
        hdr%grid%lon%size=dimLen
        rd_lon=id
      case (nc_dim_freq)
        hdr%integrate=.false.
        hdr%f%size=dimLen
        rd_freq=id
      case default
!           stop 'Error @ get_dem_dims: only 2 dimensions (lon/lat or x/y) expected!'
    end select
  end do
!
  if (hdr%grid%distinctGridValues) hdr%grid%numberOfPoints=hdr%grid%lon%size*hdr%grid%lat%size
  if (.not.halt)mask=.true.
!
  return
!
!*****************************************************************************80
End subroutine oswi_read_nc_get_dim


Subroutine oswi_read_nc_inq_var ( ncid, hdr, verbose, mask )
!*****************************************************************************80
!
!! OSWI_READ_NC_INQ_VAR
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
  integer(int32)     , intent(in)             :: ncid
  type(type_oswi_hdr), intent(inout)          :: hdr
  logical            , intent(in) , optional  :: verbose
  logical            , intent(out), optional  :: mask
!
! Local variables
!
  logical                 :: verb, halt, ok
  character(len=len_att)  :: varName, frequency_type, pre='Error @ oswi_read_nc_inq_var :'
  integer(int8)           :: logical_dummy
  integer(int32)          :: nVars, var, varDims
  integer(int32), dimension(nf90_max_var_dims)  :: varDimids
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write(output_unit,"(2x,'>> ',a)") 'Inquire oswi netcdf variables'
!
  if (present(mask))then
    halt=.false.
    mask=.false.
  else
    halt=.true.
  endif
!
! Init
!
  hdr%dem%defined=.false.
  hdr%osw%hasselmann=.true.
!
! Check number of variables
!
  call nc_check( &
    nf90_inquire(ncid, nVariables = nVars), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
!
! examine variables
!
  rv_fc=0_int32; rv_fd=0_int32; rv_fbw=0_int32; rv_frms=0_int32
!
  do var=1_int32,nVars
    call nc_check( &
      nf90_inquire_variable(ncid, varid=var, name=varName, ndims=varDims, dimids=varDimids), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    select case (varName)
      case(nc_var_epoch)
        v_time=var
      case(nc_var_time)
        v_tstr=var
      case(nc_var_lat)
        if (varDims.ne.1.and.varDimids(1).ne.rd_lat) then
           if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'illegal lat dimension!'
           if (halt) stop; return
        end if
        rv_lat=var
      case(nc_var_lon)
        rv_lon=var
        if (hdr%grid%distinctGridValues) then
           if (varDims.ne.1.and.varDimids(1).ne.rd_lon) then
              if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'illegal lon dimension!'
              if (halt) stop; return
           end if
        else
           if (varDims.ne.1.and.varDimids(1).ne.rd_nofp) then
              if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'illegal lon dimension!'
              if (halt) stop; return
           end if
        end if
      case(nc_var_freq)
        if (.not.hdr%integrate) then
          if (varDims.ne.1.and.varDimids(1).ne.rd_freq) then
             if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'illegal frequency dimension!'
             if (halt) stop; return
          end if
        end if
        rv_freq=var
        call nc_check( &
           nf90_get_att( ncid, rv_freq, nc_att_long_name, frequency_type ), &
           verbose=verb, mask=ok, halt=halt, prefix=pre &
           )
        if (.not.ok) return
      case(nc_var_freqd)
        if (.not.hdr%integrate) then
           if (verb) write(error_unit,"(a,2(1x,a))") trim(pre), trim(varName), 'should not be defined!'
           if (halt) stop; return
        end if
        rv_fd=var
      case(nc_var_freqc)
        if (.not.hdr%integrate) then
           if (verb) write(error_unit,"(a,2(1x,a))") trim(pre), trim(varName), 'should not be defined!'
           if (halt) stop; return
        end if
        rv_fc=var
        hdr%fanalysis=.true.
      case(nc_var_freqbw)
        if (.not.hdr%integrate) then
           if (verb) write(error_unit,"(a,2(1x,a))") trim(pre), trim(varName), 'should not be defined!'
           if (halt) stop; return
        end if
        rv_fbw=var
        hdr%fanalysis=.true.
      case(nc_var_freqrms)
        if (.not.hdr%integrate) then
           if (verb) write(error_unit,"(a,2(1x,a))") trim(pre), trim(varName), 'should not be defined!'
           if (halt) stop; return
        end if
        rv_frms=var
        hdr%fanalysis=.true.
      case(nc_var_pl)
        if (varDims.ne.1.and.varDimids(1).ne.rd_lat) then
           if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'illegal pl dimension!'
           if (halt) stop; return
        end if
        rv_pl=var
      case('wave_spectra_input','sea_surface_wave_spectra_input','sea_surface_wave_spectra')
        rv_wave=var
      case('bathymetry_input','depth_input')
        rv_dem=var
        hdr%dem%defined=.true.
      case(nc_var_grid)
        rv_grid=var
      case('swh')
        hdr%type='swh'
        hdr%swh=.true.
        hdr%osw%hasselmann=.false.
        hdr%name=varName
        rv_oswi=var
      case('swi')
        hdr%type='swi'
        hdr%swi=.true.
        hdr%name=varName
        rv_oswi=var
      case('p_air')
        hdr%type='air'
        hdr%air=.true.
        hdr%name=varName
        rv_oswi=var
      case('p_sea_water')
        hdr%type='sea'
        hdr%sea=.true.
        hdr%name=varName
        rv_oswi=var
      case('p_bedrock')
        hdr%type='bed'
        hdr%bed=.true.
        hdr%name=varName
        rv_oswi=var
      case('d')
        hdr%type='sfd'
        hdr%sfd=.true.
        hdr%name=varName
        rv_oswi=var
      case('depth')
        hdr%debug=.true.
        hdr%type='dem'
        hdr%name=varName
        rv_oswi=var
      case('mod_coeff')
        hdr%debug=.true.
        hdr%name=varName
        rv_oswi=var
      case default
!          print *, 'Unused variable : ', trim(varName)
    end select
  end do
!
  if (hdr%integrate.and.rv_fd.eq.0_int32) then
    if (verb) write(error_unit,"(a,2(1x,a))") trim(pre), 'dominant frequency vector not found!'
    if (halt) stop; return
  end if
  if (hdr%fanalysis.and.(rv_fc.eq.0_int32.or.rv_fbw.eq.0_int32.or.rv_frms.eq.0_int32)) then
    if (verb) write(error_unit,"(a,2(1x,a))") trim(pre), 'frequency analyis vectors not found!'
    if (halt) stop; return
  end if
!
! check frequency
!
  if (.not.hdr%integrate.and.((hdr%swh.or.hdr%swi).and.frequency_type.ne.'Sea surface wave frequency'.or. &
      .not.hdr%swh.and..not.hdr%swi.and.frequency_type.ne.'Sound frequency')) then
    if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'frequency type and name do not match!'
    if (halt) stop; return
  end if
!
! get attributes
!
! lon
!
  call nc_check(  &
    nf90_get_att( ncid, rv_lon, nc_att_valid_min, hdr%grid%lon%first ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check(  &
    nf90_get_att( ncid, rv_lon, nc_att_valid_max, hdr%grid%lon%last ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
!
! lat
!
  call nc_check(  &
    nf90_get_att( ncid, rv_lat, nc_att_valid_min, hdr%grid%lat%first ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check(  &
    nf90_get_att( ncid, rv_lat, nc_att_valid_max, hdr%grid%lat%last ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
!
! frequency
!
    call nc_check( &
      nf90_get_att( ncid, rv_freq, nc_att_min_value, hdr%f%first ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    call nc_check( &
      nf90_get_att( ncid, rv_freq, nc_att_max_value, hdr%f%last ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
  if (hdr%integrate)then
    call nc_check( &
      nf90_get_att( ncid, rv_freq, nc_att_numberOfValues, hdr%f%count ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    hdr%f%size=-1
    hdr%f%ixFirst=-1
    hdr%f%ixLast=-1
  end if
!
! grid
!
  call nc_check(  &
    nf90_get_att( ncid, rv_grid, nc_att_gridType, hdr%grid%gridType ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check(  &
    nf90_get_att( ncid, rv_grid, nc_att_earthRadius, hdr%grid%earthRadius ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check(  &
    nf90_get_att( ncid, rv_grid, nc_att_earthIsOblate, logical_dummy ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  hdr%grid%earthIsOblate=logical_dummy.eq.nf90_true
  call nc_check(  &
    nf90_get_att( ncid, rv_grid, nc_att_interpolated, logical_dummy ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  hdr%grid%interpolated=logical_dummy.eq.nf90_true
  if (hdr%grid%gridType.eq.'icosahedron') &
    call nc_check(  &
      nf90_get_att( ncid, rv_grid, nc_att_icosahedronEdge, hdr%grid%icosahedronEdge ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
!
! osw wave input (2dfd)
!
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_short_name, hdr%osw%shortName ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_units, hdr%osw%units ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_centre, hdr%osw%centre ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_dataClass, hdr%osw%dataClass ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_dataType, hdr%osw%dataType ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_dataStream, hdr%osw%dataStream ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_gridType, hdr%osw%gridType ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_versionNumber, hdr%osw%experimentVersionNumber ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_editionNumber, hdr%osw%editionNumber ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_paramId, hdr%osw%paramId ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_frequencies, hdr%osw%frequencies ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_firstFrequency, hdr%osw%firstFrequency ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_frequencyScalar, hdr%osw%frequencyScalar ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_directions, hdr%osw%directions ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_wave, nc_att_directionIncr, hdr%osw%directionIncrement ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check(  &
    nf90_get_att( ncid, rv_wave, nc_att_Hasselmann, logical_dummy ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  hdr%osw%Hasselmann=logical_dummy.eq.nf90_true
  call nc_check(  &
    nf90_get_att( ncid, rv_wave, nc_att_directionFull, logical_dummy ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  hdr%osw%directionFullCircle=logical_dummy.eq.nf90_true
!
! depth
!
  if (hdr%dem%defined) then
    call nc_check( &
      nf90_get_att( ncid, rv_dem, nc_att_node_offset, logical_dummy ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    hdr%dem%node_offset=logical_dummy.eq.nf90_true
    call nc_check( &
      nf90_get_att( ncid, rv_dem, nc_att_interpolated, logical_dummy ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    hdr%dem%interpolated=logical_dummy.eq.nf90_true
    call nc_check( &
      nf90_put_att( ncid, rv_dem, nc_att_units, hdr%dem%units ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    call nc_check( &
      nf90_put_att( ncid, rv_dem, nc_att_resample_method, hdr%dem%resampleMethod ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    call nc_check( &
      nf90_put_att( ncid, rv_dem, nc_att_title, hdr%dem%title ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    call nc_check( &
      nf90_put_att( ncid, rv_dem, nc_att_institution, hdr%dem%institution ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    call nc_check( &
      nf90_put_att( ncid, rv_dem, nc_att_source, hdr%dem%source ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    call nc_check( &
      nf90_put_att( ncid, rv_dem, nc_att_history, hdr%dem%history ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    call nc_check( &
      nf90_put_att( ncid, rv_dem, nc_att_references, hdr%dem%references ), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
  end if
!
! global
!
  call nc_check( &
    nf90_get_att( ncid, nf90_global, nc_att_history, hdr%history ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre&
    ); if (.not.ok) return
!
! oswi
!
  call nc_check( &
    nf90_get_att( ncid, rv_oswi, nc_att_long_name, hdr%long_name ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_oswi, nc_att_standard_name, hdr%standard_name ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_oswi, nc_att_units, hdr%units ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
  call nc_check( &
    nf90_get_att( ncid, rv_oswi, nc_att_missing_value, hdr%missingValue ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
!
! Set units and output (input in this case)
!
  hdr%variance=hdr%long_name(1:6).eq.'Square'
  hdr%normalize=hdr%long_name(1:10).eq.'Normalized'.or.hdr%long_name(11:20).eq.'normalized'
  hdr%dB=hdr%units(1:2).eq.'dB'
  if (hdr%dB) then
    if (hdr%units(len_trim(hdr%units)-2:len_trim(hdr%units)).eq.'µPa') then
      read(hdr%units(7:len_trim(hdr%units)-3),*) hdr%referencePressure
    else
      read(hdr%units(7:len_trim(hdr%units)-2),*) hdr%referencePressure
      hdr%referencePressure=int(hdr%referencePressure*1.d6,kind=int32)
    end if 
  else
    hdr%lg=hdr%units(1:3).eq.'log'
  end if
  hdr%Pa=hdr%air.or.hdr%sea.or.hdr%bed
  hdr%id=rv_oswi
!
! get environment variables from group
!
  if (.not.(hdr%swh.or.hdr%swi).and.rg_env.eq.-1_int32) &
    stop 'Error @ oswi_read_nc_inq_var : environment group expected but not present in file!'
  if (rg_env.ne.-1_int32) then
    call oswi_init_env( hdr%env )
    call nc_check( &
      nf90_inquire(rg_env, nVariables = nVars), &
      verbose=verb, mask=ok, halt=halt, prefix=pre &
      ); if (.not.ok) return
    do var=1_int32,nVars
      call nc_check( &
        nf90_inquire_variable(rg_env, varid=var, name=varName), &
        verbose=verb, mask=ok, halt=halt, prefix=pre &
        )
      if (.not.ok) return
      if (varName.eq.hdr%env%rho_air%name) then
        rv_rho_air=var
      elseif (varName.eq.hdr%env%rho_sea%name) then
        rv_rho_sea=var
      elseif (varName.eq.hdr%env%rho_bed%name) then
        rv_rho_bed=var
      elseif (varName.eq.hdr%env%c_air%name) then
        rv_c_air=var
      elseif (varName.eq.hdr%env%c_sea%name) then
        rv_c_sea=var
      elseif (varName.eq.hdr%env%c_bed%name) then
        rv_c_bed=var
      end if
    end do
  end if
!
  if (.not.halt)mask=.true.
  return
!
!*****************************************************************************80
End subroutine oswi_read_nc_inq_var


Subroutine oswi_read_nc_get_hdr ( ncid, hdr, verbose )
!*****************************************************************************80
!
!! OSWI_READ_NC_GET_HDR
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
  integer(int32)     , intent(in)               :: ncid
  type(type_oswi_hdr), intent(inout), target    :: hdr
  logical            , intent(in)   , optional  :: verbose
!
! Local variables
!
  logical         :: verb
  real(double)    :: swap
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write(output_unit,"(2x,'>> ',a)") 'Get netcdf header variables'
!
! allocate
!
!
  if (hdr%grid%distinctGridValues) then
!   lat
    if (allocated(hdr%grid%lat%dat).and.size(hdr%grid%lat%dat).ne.hdr%grid%lat%size) &
      & deallocate(hdr%grid%lat%dat)
    if (.not.allocated(hdr%grid%lat%dat)) allocate(hdr%grid%lat%dat(hdr%grid%lat%size))
!   lon
    if (allocated(hdr%grid%lon%dat).and.size(hdr%grid%lon%dat).ne.hdr%grid%lon%size) &
      & deallocate(hdr%grid%lon%dat)
    if (.not.allocated(hdr%grid%lon%dat)) allocate(hdr%grid%lon%dat(hdr%grid%lon%size))
  else
!   lon
    if (allocated(hdr%grid%lon%dat).and.size(hdr%grid%lon%dat).ne.hdr%grid%numberOfPoints) &
      & deallocate(hdr%grid%lon%dat)
    if (.not.allocated(hdr%grid%lon%dat)) allocate(hdr%grid%lon%dat(hdr%grid%numberOfPoints))
!
    if (hdr%grid%gridType.eq.'reduced_ll') then
!     lat
      if (allocated(hdr%grid%lat%dat).and.size(hdr%grid%lat%dat).ne.hdr%grid%lat%size) &
        & deallocate(hdr%grid%lat%dat)
      if (.not.allocated(hdr%grid%lat%dat)) allocate(hdr%grid%lat%dat(hdr%grid%lat%size))
!     pl      
      if (allocated(hdr%grid%pl).and.size(hdr%grid%pl).ne.hdr%grid%lat%size) deallocate(hdr%grid%pl)
      if (.not.allocated(hdr%grid%pl)) allocate(hdr%grid%pl(hdr%grid%lat%size))
!      lat mask
      if (allocated(hdr%grid%lat%mask).and.size(hdr%grid%lat%mask).ne.hdr%grid%lat%size) &
        & deallocate(hdr%grid%lat%mask)
      if (.not.allocated(hdr%grid%lat%mask)) allocate(hdr%grid%lat%mask(hdr%grid%lat%size))
    elseif (hdr%grid%gridType.eq.'icosahedron') then
!     lat
      if (allocated(hdr%grid%lat%dat).and.size(hdr%grid%lat%dat).ne.hdr%grid%numberOfPoints) &
        & deallocate(hdr%grid%lat%dat)
      if (.not.allocated(hdr%grid%lat%dat)) allocate(hdr%grid%lat%dat(hdr%grid%numberOfPoints))
    end if
  end if
!
  if (.not.hdr%integrate) then
!    frequency
    if (allocated(hdr%f%sound).and.size(hdr%f%sound,1).ne.hdr%f%size) deallocate(hdr%f%sound)
    if (.not.allocated(hdr%f%sound)) allocate(hdr%f%sound(hdr%f%size))
    if (allocated(hdr%f%wave).and.size(hdr%f%sound,1).ne.hdr%f%size) deallocate(hdr%f%wave)
    if (.not.allocated(hdr%f%wave)) allocate(hdr%f%wave(hdr%f%size))
    if (allocated(hdr%f%mask).and.size(hdr%f%mask,1).ne.hdr%f%size) deallocate(hdr%f%mask)
    if (.not.allocated(hdr%f%mask)) allocate(hdr%f%mask(hdr%f%size))
  end if
!
! Grid
!
  call nc_check( nf90_get_var( ncid, rv_lon, hdr%grid%lon%dat ) )
  hdr%grid%lon%reversed=hdr%grid%lon%dat(1).gt.hdr%grid%lon%dat(2)
  if (hdr%grid%lon%reversed) then
    swap=hdr%grid%lon%first
    hdr%grid%lon%first=hdr%grid%lon%last
    hdr%grid%lon%last=swap
  end if
  hdr%grid%lon%count=hdr%grid%lon%size
  hdr%grid%lon%range=range(hdr%grid%lon%dat)
  if (hdr%grid%distinctGridValues) then
    hdr%grid%lon%increment=abs(hdr%grid%lon%dat(2)-hdr%grid%lon%dat(1))
    hdr%grid%lon%ixFirst=1_int32
    hdr%grid%lon%ixLast=hdr%grid%lon%size
  else
    hdr%grid%lon%increment=-1._double
    hdr%grid%lon%ixFirst=-1_int32
    hdr%grid%lon%ixLast=-1_int32
  end if
!
  call nc_check( nf90_get_var( ncid, rv_lat, hdr%grid%lat%dat ) )
  hdr%grid%lat%reversed=hdr%grid%lat%dat(1).gt.hdr%grid%lat%dat(2)
  if (hdr%grid%lat%reversed) then
    swap=hdr%grid%lat%first
    hdr%grid%lat%first=hdr%grid%lat%last
    hdr%grid%lat%last=swap
  end if
  hdr%grid%lat%count=hdr%grid%lat%size
  hdr%grid%lat%range=range(hdr%grid%lat%dat)
  hdr%grid%lat%increment=abs(hdr%grid%lat%dat(2)-hdr%grid%lat%dat(1))
  hdr%grid%lat%ixFirst=1_int32
  hdr%grid%lat%ixLast=hdr%grid%lat%size
!
  if (hdr%grid%gridType.eq.'reduced_ll') then
    call nc_check( nf90_get_var( ncid, rv_pl, hdr%grid%pl ) )
    hdr%grid%lat%mask=hdr%grid%pl.gt.0_int32
    hdr%grid%lat%count=count(hdr%grid%lat%mask)
    if (hdr%grid%lat%reversed) then
      hdr%grid%lat%ixFirst=maxloc(hdr%grid%lat%dat,dim=1,mask=hdr%grid%lat%mask)
      hdr%grid%lat%ixLast=minloc(hdr%grid%lat%dat,dim=1,mask=hdr%grid%lat%mask)
    else
      hdr%grid%lat%ixFirst=minloc(hdr%grid%lat%dat,dim=1,mask=hdr%grid%lat%mask)
      hdr%grid%lat%ixLast=maxloc(hdr%grid%lat%dat,dim=1,mask=hdr%grid%lat%mask)
    end if
  end if
!
! Unpack grid
!
  call oswi_unpack_grid(hdr%grid,verbose)
!
! Frequency
!
  if (.not.hdr%integrate) then
    if (hdr%swh.or.hdr%swi) then
      call nc_check( nf90_get_var( ncid, rv_freq, hdr%f%wave ) )
      hdr%f%sound=hdr%f%wave*2
      hdr%f%dat=>hdr%f%wave
    else
      call nc_check( nf90_get_var( ncid, rv_freq, hdr%f%sound ) )
      hdr%f%wave=hdr%f%sound/2
      hdr%f%dat=>hdr%f%sound
    end if
    hdr%f%mask=.true.
    hdr%f%count=hdr%f%size
  end if
!
! Environment
!
  if (.not.hdr%swh.and..not.hdr%swi) then
    call nc_check( nf90_get_var( rg_env, rv_rho_air, hdr%env%rho_air%value ) )
    call nc_check( nf90_get_var( rg_env, rv_rho_sea, hdr%env%rho_sea%value ) )
    call nc_check( nf90_get_var( rg_env, rv_rho_bed, hdr%env%rho_bed%value ) )
    call nc_check( nf90_get_var( rg_env, rv_c_air, hdr%env%c_air%value ) )
    call nc_check( nf90_get_var( rg_env, rv_c_sea, hdr%env%c_sea%value ) )
    call nc_check( nf90_get_var( rg_env, rv_c_bed, hdr%env%c_bed%value ) )
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_read_nc_get_hdr


Subroutine oswi_read_dat_ll ( ncid, hdr, dat_ll, verbose, mask )
!*****************************************************************************80
!
!! OSWI_READ_DAT_LL
!
!  Description:
!
!    Get integrated oswi ll data array from netcdf4 file
!
!  Modified:
!
!    16 January 2017
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
  integer(int32)        , intent(in)              :: ncid
  type(type_oswi_hdr)   , intent(in)              :: hdr
  type(type_oswi_dat_ll), intent(inout)           :: dat_ll
  logical            , intent(in) , optional      :: verbose
  logical            , intent(out), optional      :: mask
!
! Local variables
!
  logical                 :: verb, halt, ok
  character(len=len_att)  :: pre='Error @ oswi_read_dat_ll :'
  real(single), dimension(:,:), allocatable  :: regular_ll
  real(single), dimension(:)  , pointer      :: dat2reg_ll
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write(output_unit,"(2x,'>> ',a)") 'Get oswi dat_ll from nc file'
!
  if (present(mask)) then
    halt=.false.
    mask=.false.
  else
    halt=.true.
  endif
!
! check
!
  if (.not.hdr%integrate) then
    if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'dat_ll expected but dat_llf in nc file!'
    if (halt) stop; return
  end if
  if (hdr%id.le.0_int32) then
    if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'oswi id not set!'
    if (halt) stop; return
  end if
  if (size(dat_ll%dat,1).ne.hdr%grid%numberOfPoints) then
    if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'dat_ll wrong dimensions!'
    if (halt) stop; return
  end if
  if (size(dat_ll%mask,1).ne.hdr%grid%numberOfPoints) then
    if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'dat_mask wrong dimensions!'
    if (halt) stop; return
  end if
!
! get maxValue
!
  call nc_check( &
    nf90_get_att( ncid, hdr%id, nc_att_max_value, dat_ll%maxValue ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
!
! Get oswi data
!
  if (hdr%grid%distinctGridValues) then
!
    allocate( regular_ll(hdr%grid%lon%size,hdr%grid%lat%size) )
!
    call nc_check( nf90_get_var( ncid, hdr%id, regular_ll ) )
    dat2reg_ll => oswi_read_reshape_ll( regular_ll, (/hdr%grid%numberOfPoints/) )
    dat_ll%dat=real(dat2reg_ll,kind=double)
!
    if (allocated(dat_ll%fc).and.rv_fc.ne.0_int32) then
      call nc_check( nf90_get_var( ncid, rv_fc, regular_ll ) )
      dat2reg_ll => oswi_read_reshape_ll( regular_ll, (/hdr%grid%numberOfPoints/) )
      dat_ll%fc=real(dat2reg_ll,kind=double)
    end if
!
    if (allocated(dat_ll%fd).and.rv_fd.ne.0_int32) then
      call nc_check( nf90_get_var( ncid, rv_fd, regular_ll ) )
      dat2reg_ll => oswi_read_reshape_ll( regular_ll, (/hdr%grid%numberOfPoints/) )
      dat_ll%fd=real(dat2reg_ll,kind=double)
    end if
!
    if (allocated(dat_ll%fb).and.rv_fbw.ne.0_int32) then
      call nc_check( nf90_get_var( ncid, rv_fbw, regular_ll ) )
      dat2reg_ll => oswi_read_reshape_ll( regular_ll, (/hdr%grid%numberOfPoints/) )
      dat_ll%fb=real(dat2reg_ll,kind=double)
    end if
!
    if (allocated(dat_ll%fr).and.rv_frms.ne.0_int32) then
      call nc_check( nf90_get_var( ncid, rv_frms, regular_ll ) )
      dat2reg_ll => oswi_read_reshape_ll( regular_ll, (/hdr%grid%numberOfPoints/) )
      dat_ll%fr=real(dat2reg_ll,kind=double)
    end if
!
    deallocate( regular_ll )
    nullify( dat2reg_ll )
!
  else
    call nc_check( nf90_get_var( ncid, hdr%id, dat_ll%dat ) )
    if (allocated(dat_ll%fc).and.rv_fc.ne.0_int32) call nc_check( nf90_get_var( ncid, rv_fc, dat_ll%fc ) )
    if (allocated(dat_ll%fd).and.rv_fd.ne.0_int32) call nc_check( nf90_get_var( ncid, rv_fd, dat_ll%fd ) )
    if (allocated(dat_ll%fb).and.rv_fbw.ne.0_int32) call nc_check( nf90_get_var( ncid, rv_fbw, dat_ll%fb ) )
    if (allocated(dat_ll%fr).and.rv_frms.ne.0_int32) call nc_check( nf90_get_var( ncid, rv_frms, dat_ll%fr ) )
  end if
!
! Convert to base units
!
  call oswi_convert_dat_ll_input ( hdr, dat_ll, verb )
!
  if (.not.halt) mask=.true.
  return
!
!*****************************************************************************80
End subroutine oswi_read_dat_ll


Subroutine oswi_read_dat_llf ( ncid, hdr, dat_llf, verbose, mask )
!*****************************************************************************80
!
!! OSWI_READ_DAT_LLF
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
  integer(int32)         , intent(in)             :: ncid
  type(type_oswi_hdr)    , intent(in)             :: hdr
  type(type_oswi_dat_llf), intent(inout)          :: dat_llf
  logical                , intent(in) , optional  :: verbose
  logical                , intent(out), optional  :: mask
!
! Local variables
!
  logical                 :: verb, halt, ok
  character(len=len_att)  :: pre='Error @ oswi_read_dat_llf :'
  real(single), dimension(:,:,:), allocatable  :: regular_llf
  real(single), dimension(:,:)  , pointer      :: dat2reg_llf
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write(output_unit,"(2x,'>> ',a)") 'Get oswi dat_llf from nc file'
!
  if (present(mask))then
    halt=.false.
    mask=.false.
  else
    halt=.true.
  endif
!
! check
!
  if (hdr%integrate) then
    if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'dat_llf expected but dat_ll in nc file!'
    if (halt) stop; return
  end if
  if (hdr%id.le.0_int32) then
    if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'oswi id not set!'
    if (halt) stop; return
  end if
  if (size(dat_llf%dat,1).ne.hdr%grid%numberOfPoints.or.size(dat_llf%dat,2).ne.hdr%f%size) then
    if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'dat_llf wrong dimensions!'
    if (halt) stop; return
  end if
  if (size(dat_llf%mask,1).ne.hdr%grid%numberOfPoints.or.size(dat_llf%mask,2).ne.hdr%f%size) then
    if (verb) write(error_unit,"(a,1x,a)") trim(pre), 'dat_mask wrong dimensions!'
    if (halt) stop; return
  end if
!
! get maxValue
!
  call nc_check( &
    nf90_get_att( ncid, hdr%id, nc_att_max_value, dat_llf%maxValue ), &
    verbose=verb, mask=ok, halt=halt, prefix=pre &
    ); if (.not.ok) return
!
! Get oswi data
!
  if (hdr%grid%distinctGridValues) then
    allocate( regular_llf(hdr%grid%lon%size,hdr%grid%lat%size,hdr%f%size) )
    call nc_check( nf90_get_var( ncid, hdr%id, regular_llf ) )
    dat2reg_llf => oswi_read_reshape_llf( regular_llf, (/hdr%grid%numberOfPoints,hdr%f%size/) )
    dat_llf%dat=real(dat2reg_llf,kind=double)
    deallocate( regular_llf )
    nullify( dat2reg_llf )
  else
    call nc_check( nf90_get_var( ncid, hdr%id, dat_llf%dat ) )
  end if
!
! Convert to base units
!
  call oswi_convert_dat_llf_input ( hdr, dat_llf, verb )
!
  if (.not.halt) mask=.true.
  return
!
!*****************************************************************************80
End subroutine oswi_read_dat_llf


Subroutine oswi_read_dat ( ncid, hdr, dat, verbose, mask )
!*****************************************************************************80
!
!! OSWI_READ_NC_GET_DAT
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
  integer(int32)     , intent(in)             :: ncid
  type(type_oswi_hdr), intent(in)             :: hdr
  type(type_oswi_dat), intent(inout)          :: dat
  logical            , intent(in) , optional  :: verbose
  logical            , intent(out), optional  :: mask
!
! Local variables
!
  logical                 :: verb, halt
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
!  if (verb) write(output_unit,"(2x,'>> ',a)") 'Get oswi dat from nc file'
!
  if (present(mask))then
    halt=.false.
    mask=.false.
  else
    halt=.true.
  endif
!
! get time information
!
  call nc_check( nf90_get_var( ncid, v_time, dat%epoch ) )
  call nc_check( nf90_get_var( ncid, v_tstr, dat%time ) )
!
! allocate
!
  call liboswi_allocate_type_dat( hdr, dat )
!
  if (hdr%integrate) then
    call oswi_read_dat_ll ( ncid, hdr, dat%ll, verbose, mask )
  else
    call oswi_read_dat_llf ( ncid, hdr, dat%llf, verbose, mask )
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_read_dat


Function oswi_read_reshape_ll(array, shape_) result(aptr)
!*****************************************************************************80
!
!! OSWI_READ_RESHAPE_LL
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
  real(single), dimension(:,:), intent(in), target :: array
  integer(int32), intent(in), dimension(1) :: shape_
  real(single), dimension(:), pointer :: aptr
! Use C_LOC to get the start location of the array data, and
! use C_F_POINTER to turn this into a fortran pointer (aptr).
! Note that we need to specify the shape of the pointer using an
! integer array.
  call C_F_POINTER(C_LOC(array), aptr, shape_)
!
!*****************************************************************************80
End function oswi_read_reshape_ll


Function oswi_read_reshape_llf(array, shape_) result(aptr)
!*****************************************************************************80
!
!! OSWI_READ_RESHAPE_LLF
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
  real(single), dimension(:,:,:), intent(in), target :: array
  integer(int32), intent(in), dimension(2) :: shape_
  real(single), dimension(:,:), pointer :: aptr
! Use C_LOC to get the start location of the array data, and
! use C_F_POINTER to turn this into a fortran pointer (aptr).
! Note that we need to specify the shape of the pointer using an
! integer array.
  call C_F_POINTER(C_LOC(array), aptr, shape_)
!
!*****************************************************************************80
End function oswi_read_reshape_llf
