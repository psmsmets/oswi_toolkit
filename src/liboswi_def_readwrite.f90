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
!  Description:  liboswi read/write definitions include
!
!
!*****************************************************************************80
!
! define attributes
!
  character(len=*), parameter, private :: nc_att_axis            = "axis"
  character(len=*), parameter, private :: nc_att_units           = "units"
  character(len=*), parameter, private :: nc_att_long_name       = "long_name"
  character(len=*), parameter, private :: nc_att_standard_name   = "standard_name"
  character(len=*), parameter, private :: nc_att_short_name      = "short_name"
  character(len=*), parameter, private :: nc_att_valid_min       = "valid_min"
  character(len=*), parameter, private :: nc_att_valid_max       = "valid_max"
  character(len=*), parameter, private :: nc_att_min_value       = "min_value"
  character(len=*), parameter, private :: nc_att_max_value       = "max_value"
  character(len=*), parameter, private :: nc_att_valid_range     = "valid_range"
  character(len=*), parameter, private :: nc_att_numberOfValues  = "numberOfValues"
  character(len=*), parameter, private :: nc_att_scale_factor    = "scale_factor"
  character(len=*), parameter, private :: nc_att_add_offset      = "add_offset"
  character(len=*), parameter, private :: nc_att_fill_value      = "_FillValue"
  character(len=*), parameter, private :: nc_att_missing_value   = "missing_value"
  character(len=*), parameter, private :: nc_att_signedness      = "signedness"
!
  character(len=*), parameter, private :: nc_att_title           = "title"
  character(len=*), parameter, private :: nc_att_institution     = "institution"
  character(len=*), parameter, private :: nc_att_source          = "source"
  character(len=*), parameter, private :: nc_att_references      = "references"
  character(len=*), parameter, private :: nc_att_history         = "history"
  character(len=*), parameter, private :: nc_att_Conventions     = "Conventions"
  character(len=*), parameter, private :: nc_att_featureType     = "featureType"
  character(len=*), parameter, private :: nc_att_comment         = "comment"
  character(len=*), parameter, private :: nc_att_version         = "version"
  character(len=*), parameter, private :: nc_att_created         = "created"
!
  character(len=*), parameter, private :: nc_att_centre          = "centre"
  character(len=*), parameter, private :: nc_att_dataClass       = "dataClass"
  character(len=*), parameter, private :: nc_att_dataType        = "dataType"
  character(len=*), parameter, private :: nc_att_dataStream      = "dataStream"
  character(len=*), parameter, private :: nc_att_gridType        = "gridType"
  character(len=*), parameter, private :: nc_att_versionNumber   = "experimentVersionNumber"
  character(len=*), parameter, private :: nc_att_editionNumber   = "editionNumber"
  character(len=*), parameter, private :: nc_att_paramId         = "paramId"
  character(len=*), parameter, private :: nc_att_earthIsOblate   = "earthIsOblate"
  character(len=*), parameter, private :: nc_att_icosahedronEdge = "icosahedronEdge"
  character(len=*), parameter, private :: nc_att_earthRadius     = "earthRadius"
  character(len=*), parameter, private :: nc_att_frequencies     = "frequencies"
  character(len=*), parameter, private :: nc_att_firstFrequency  = "firstFrequency"
  character(len=*), parameter, private :: nc_att_frequencyScalar = "frequencyScalar"
  character(len=*), parameter, private :: nc_att_directions      = "directions"
  character(len=*), parameter, private :: nc_att_directionIncr   = "directionIncrement"
  character(len=*), parameter, private :: nc_att_directionFull   = "directionFullCircle"
  character(len=*), parameter, private :: nc_att_Hasselmann      = "HasselmannIntegral"
!
  character(len=*), parameter, private :: nc_att_node_offset     = "node_offset"
  character(len=*), parameter, private :: nc_att_interpolated    = "interpolated"
  character(len=*), parameter, private :: nc_att_integrated      = "integrated"
  character(len=*), parameter, private :: nc_att_resample_method = "resample_method"
!
! define variable and dimension names
!
  character(len=*), parameter, private :: nc_var_time    = 'time'
  character(len=*), parameter, private :: nc_var_epoch   = 'epoch'
  character(len=*), parameter, private :: nc_var_jday    = 'julianday'
  character(len=*), parameter, private :: nc_var_lon     = 'lon'
  character(len=*), parameter, private :: nc_var_lat     = 'lat'
  character(len=*), parameter, private :: nc_var_pl      = 'pl'
  character(len=*), parameter, private :: nc_var_freq    = 'frequency'
  character(len=*), parameter, private :: nc_var_grid    = 'grid'
  character(len=*), parameter, private :: nc_var_mars    = 'mars'
  character(len=*), parameter, private :: nc_var_wave    = 'wave_spectra_input'
  character(len=*), parameter, private :: nc_var_dem     = 'dem_bathymetry_input'
  character(len=*), parameter, private :: nc_var_freqd   = 'f_dom'
  character(len=*), parameter, private :: nc_var_freqc   = 'f_cen'
  character(len=*), parameter, private :: nc_var_freqbw  = 'f_bw'
  character(len=*), parameter, private :: nc_var_freqrms = 'f_rms'
  !
  character(len=*), parameter, private :: nc_dim_lat     = nc_var_lat
  character(len=*), parameter, private :: nc_dim_lon     = nc_var_lon
  character(len=*), parameter, private :: nc_dim_freq    = nc_var_freq
  character(len=*), parameter, private :: nc_dim_points  = 'numberOfPoints'
!
! adaguc
!
!!  integer(int32), private :: v_iso, v_prod, v_proj
!
! time parameters
!
  integer(int32), private :: v_time, v_tstr, v_tmat
!
  character(len=*), parameter, private :: s_time = "epoch"
  character(len=*), parameter, private :: l_time = "Epoch time"
  character(len=*), parameter, private :: u_time = time_units
!
  character(len=*), parameter, private :: s_tstr = "time"
  character(len=*), parameter, private :: l_tstr = "Date time string"
  character(len=*), parameter, private :: u_tstr = tstr_units 
!
  character(len=*), parameter, private :: s_tmat = "julianday"
  character(len=*), parameter, private :: l_tmat = "Julian day number"
  character(len=*), parameter, private :: u_tmat = matlab_time_units 
!
! define character-position dimension for strings of max length 40
!
  character(len=*), parameter, private :: s_strLen  = "StrLen"
  character(len=*), parameter, private :: l_strLen  = "Character variable maximum string length (Fortran)"
  character(len=*), parameter, private :: u_strLen  = "-"
  integer(int32), private, parameter   :: strLen    = 40
  integer(int32), private, parameter   :: nc_strLen = 40
  integer(int32), private :: d_strLen
!
! define dimensions
!
  integer(int32), private :: rd_freq, rd_lon, rd_lat, rd_nofp
  integer(int32), private :: rv_freq, rv_lon, rv_lat, rv_pl, rv_ed
  integer(int32), private :: rv_fc, rv_fd, rv_fbw, rv_frms
!
  integer(int32), private :: wd_freq, wd_lon, wd_lat, wd_nofp
  integer(int32), private :: wv_freq, wv_lon, wv_lat, wv_pl, wv_ed
  integer(int32), private :: wv_fc, wv_fd, wv_fbw, wv_frms
!
! oswi wave and dem info
!
  integer(int32), private :: rv_oswi, rv_wave, rv_dem, rv_grid
  integer(int32), private :: rv_rho_air, rv_rho_sea, rv_rho_bed
  integer(int32), private :: rv_c_air, rv_c_sea, rv_c_bed
!
  integer(int32), private :: wv_oswi, wv_wave, wv_dem, wv_grid
  integer(int32), private :: wv_rho_air, wv_rho_sea, wv_rho_bed
  integer(int32), private :: wv_c_air, wv_c_sea, wv_c_bed 
!
! groups
!
  integer(int32), private :: rg_env, wg_env
