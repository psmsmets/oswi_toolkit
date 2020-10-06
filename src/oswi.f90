Program main 
!*****************************************************************************80
!
!! OSWI - Ocean Surface Wave Interaction 
!
!  Description:
!
!   SSWINT determines the source strength spectrum, integrated over a discrete
!   frequency range, providing the ECMWF WAM (Wave Model) HRES 2D
!   ocean wave directional energy spectra (2DFD). Optionally bathymetry can be
!   used to correct the source strength for bathymetry excitation (resonance).
!  
!   SSWINT generates a 2d netcdf file (.grd) with same name as the input grib
!   file, with additional prefix and suffix. If a folder is provided containing
!   multiple grib files, make sure the file characteristics are identical: same
!   grid parameters, directions and frequencies!
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!
!****************************
!  
  use io_types
  use string_utility
  use file_utility
  use sort
  use time
  use libdem
  use grib_2dfd 
  use liboswi
  use omp_lib
!
  implicit none
! 
! declare varibles
!
  type(type_oswi)     , target :: oswi, oswi_dummy 
  type(type_dem)      , target :: dem
  type(type_grib_2dfd), target :: ow2dfd
!
  integer(int32), parameter  :: maxinfs    = 999_int32
  integer(int32), parameter  :: infssize   = 256_int32
  integer(int32), parameter  :: charShort  =  64_int32
!
  logical         :: exists, overwrite, parallel, verb, verbmore, add_to_history, debug_bathymetry, debug, &
                     quick, set_in_reference, in_nc, in_grib, modulate, integrate, &
                     grid_convert, grid_regular_ll, grid_icosahedron 
  integer(int32)  :: i, inf, nofinfs, io, threads, max_threads, hours, minutes
  integer(int64)  :: epoch
  real(double)    :: proc_time, seconds, dummy_double
!
  logical                , dimension(maxinfs)  :: infs_mask
  integer(int32)         , dimension(maxinfs)  :: ix
  integer(int64)         , dimension(maxinfs)  :: infs_epoch
  character(len=infssize), dimension(maxinfs)  :: infs 
  character(len=infssize)                      :: inf_dummy, arg_type, arg_value, arg_value2
  character(len=infssize)                      :: filename
  character(len=maxinfs)                       :: ncfile
  character(len=charShort)                     :: ext
!
  real(single), dimension(:), allocatable      :: depth
!
! ----------------------------------------------------------------------------80
!
! Get start time
!
! ----------------------------------------------------------------------------80
!
  proc_time = omp_get_wtime() 
!
! ----------------------------------------------------------------------------80
!
! Set default parameters
!
! ----------------------------------------------------------------------------80
!
  include 'oswi_defaults.f90'
!
! ----------------------------------------------------------------------------80
!
! Get command line arguments and update output
!
! ----------------------------------------------------------------------------80
!
  include 'oswi_argc.f90'
  call oswi_set_output ( oswi )
!
! ----------------------------------------------------------------------------80
!
! Preliminary checks
!
! ----------------------------------------------------------------------------80
!
! check nofinfs 
!
  if ( nofinfs.lt.1_int32 ) then
    write (error_unit,"(a)") 'Error: no grib or netcdf files provided!'
    stop 1 
  end if
!
! check solid and infinite ocean
!
  if (oswi%hdr%bed.and..not.oswi%hdr%dem%defined) then
    write (error_unit,"(a)") 'Error: Conflict of argument options "--bedrock" and "--infinite-ocean". &
      & Microseism source intensity modelling requires a finite ocean!'
    stop 1
  end if
!
! No need for bathymetry?
!
  if (oswi%hdr%swh.or.oswi%hdr%swi) oswi%hdr%dem%defined=.false.
!
! ----------------------------------------------------------------------------80
!
! Parallelize
!
! ----------------------------------------------------------------------------80
!
  if (parallel) call omp_set_num_threads( threads )
!
! ----------------------------------------------------------------------------80
!
! Print header
!
! ----------------------------------------------------------------------------80
!
  if (verb) then
    write (output_unit,"(a)") ''
    write (output_unit,"(a)") 'OSWI - Ocean Surface Wave Interaction'
    write (output_unit,"(a)") ''
    if (oswi%hdr%debug) then
      if (     debug_bathymetry) write (output_unit,"(a)") '// Export bathymetry //'
      if (.not.debug_bathymetry) write (output_unit,"(a)") '// Export modulation coefficients //'
    else
      if (oswi%hdr%swh) write (output_unit,"(3a)") '// Significant Wave Height //'
      if (oswi%hdr%swi) write (output_unit,"(3a)") '// Surface Wave Interaction (Hasselmann) //'
      if (oswi%hdr%air) write (output_unit,"(3a)") '// Sound pressure in air //'
      if (oswi%hdr%sea) write (output_unit,"(3a)") '// Sound pressure in sea water //'
      if (oswi%hdr%bed) write (output_unit,"(3a)") '// Sound pressure in bedrock //'
      if (oswi%hdr%sfd) write (output_unit,"(3a)") '// Sea floor deformation (Rayleigh wave) //'
    end if
    write (output_unit,"('> ',a)")  'Options'
    write (output_unit,"(4x,a,l1)") 'Overwrite              = ', overwrite
    if (quick) write (output_unit,"(4x,a)") 'Quick and dirty        = T'
    if (threads.gt.1) write (output_unit,"(4x,a,i4)") 'OpenMP threads         = ', threads
    write (output_unit,"(4x,2a)") 'filename               = ', trim(filename)
    if (oswi%hdr%variance  ) write (output_unit,"(4x,a)") 'variance               = T'
    if (oswi%hdr%normalize ) write (output_unit,"(4x,a)") 'normalize              = T'
    if (oswi%hdr%integrate ) write (output_unit,"(4x,a)") 'integrate              = T'
    if (.not.debug_bathymetry) then
      write (output_unit,"('> ',a)") 'Set environment'
      if (     oswi%hdr%dem%defined ) write (output_unit,"(4x,a)") &
        'ocean-depth            = finite (specified by DEM)'
      if (.not.oswi%hdr%dem%defined ) write (output_unit,"(4x,a)") &
        'ocean-depth            = infinite'
      if (.not.oswi%hdr%swh.and..not.oswi%hdr%swi) then
        write (output_unit,"(4x,a,f11.5,a)") 'air density            = ', &
          oswi%hdr%env%rho_air%value, ' kg m-3'
        write (output_unit,"(4x,a,f11.5,a)") 'air velocity           = ', &
          oswi%hdr%env%c_air%value,   '  m s-1'
        write (output_unit,"(4x,a,f11.5,a)") 'sea water density      = ', &
          oswi%hdr%env%rho_sea%value, ' kg m-3'
        write (output_unit,"(4x,a,f11.5,a)") 'sea water velocity     = ', &
          oswi%hdr%env%c_sea%value,   '  m s-1'
        write (output_unit,"(4x,a,f11.5,a)") 'bedrock density        = ', &
          oswi%hdr%env%rho_bed%value, ' kg m-3'
        write (output_unit,"(4x,a,f11.5,a)") 'bedrock shear velocity = ', &
          oswi%hdr%env%c_bed%value,   '  m s-1'
      end if
    end if
  end if
!     
! ----------------------------------------------------------------------------80
!
! Evaluate input files 
!
! ----------------------------------------------------------------------------80
!
  write (inf_dummy,"(i6)") nofinfs
  include 'liboswi_in.f90'
!
  if (debug) write (output_unit,"('> ',a)") 'Oswint grid info'
  if (debug) call oswi_print_grid_info ( oswi%hdr%grid, "4x," )
!
! verbose more
!
  verbmore = verb.and.count(infs_mask).eq.1_int32
  if (debug) verbmore=.true.
!     
! ----------------------------------------------------------------------------80
!
! COMMON THINGS
!
! ----------------------------------------------------------------------------80
!
! set what to do when
!
  modulate=modulate.and.(.not.oswi%hdr%swh.and..not.oswi%hdr%swi).or.oswi%hdr%debug
  integrate=integrate.and.oswi%hdr%integrate
!
! Set modulation coefficients
!
  include 'liboswi_modulate.f90'
!
! Init grid conversion
!
  include 'liboswi_grid_transform.f90'
!
! debug bathymetry or modulation coefficients?
!
  include 'liboswi_debug_bathymetry_modulation.f90'
!     
! ----------------------------------------------------------------------------80
!
! PROCESS GRIB_FILES
!
! ----------------------------------------------------------------------------80
!
  if (verb.and..not.oswi%hdr%debug) then
    write (output_unit,"('> ',2a)") 'Calculate ', strlowcase(trim(oswi%hdr%long_name)) 
    if (oswi%hdr%swh.or.oswi%hdr%swi) then
      write (output_unit,"(4x,a,2(f8.5,a),i2,a)") 'wave frequency         = ', &
        oswi%hdr%f%wave(oswi%hdr%f%ixFirst), ' Hz to ', &
        oswi%hdr%f%wave(oswi%hdr%f%ixLast), ' Hz (#', oswi%hdr%f%count ,')'
    else
      write (output_unit,"(4x,a,2(f8.5,a),i2,a)") 'sound frequency        = ', &
        oswi%hdr%f%sound(oswi%hdr%f%ixFirst), ' Hz to ', &
        oswi%hdr%f%sound(oswi%hdr%f%ixLast), ' Hz (#', oswi%hdr%f%count ,')'
    end if
    write (output_unit,"(4x,2a)") 'NetCDF variable name   = ', trim(oswi%hdr%name)
    write (output_unit,"(4x,2a)") 'units                  = ', trim(oswi%hdr%units)
    write (output_unit,"(4x,2a)") 'long name              = ', trim(oswi%hdr%long_name)
  end if
!
  do inf=1_int32, nofinfs
!
!   Make (new) ncfile?
!
    include 'liboswi_ncfile.f90'
!
!   Get WAM data and perform Hasselmann / spectrum integral
!
    if (in_grib) then
      if (verbmore) then
        if (     oswi%hdr%swh) write (output_unit,"('> ',2a)") &
          'Get 2d directional wave spectra and integrate over direction'
        if (.not.oswi%hdr%swh) write (output_unit,"('> ',2a)") &
          'Get 2d directional wave spectra and perform Hasselmann integral'
      end if
      call grib_2dfd_get_integrated_spectrum_data ( &
        gribFile       = trim(infs(inf)), &
        wam2dfd        = ow2dfd, &
        fspec          = oswi%dat%llf%dat, &
        mask           = oswi%dat%llf%mask, &
        hasselmann     = oswi%hdr%osw%hasselmann, &
        forecastStep   = 1_int32, &
        ensembleNumber = 1_int32, &
        verbose        = debug, &
        experimentVersionNumber = oswi%hdr%osw%experimentVersionNumber, &
        epoch = epoch &
        )
      if (epoch.ne.oswi%dat%epoch) write (output_unit,'(3a)') &
        'Warning: grib_2dfd_get_integrated_spectrum_data epoch does not match infs_epoch(inf) for file "', &
        trim(infs(inf)), '".'
!
!   Read oswi netcdf data
!
    else
      call oswi_read_nc_data ( &
        nc          = trim(infs(inf)), &
        dat         = oswi%dat, &       
        mask        = infs_mask(inf), & 
        debug       = debug &
        )
      if (.not.infs_mask(inf)) cycle
    end if
!
!   Calculate source strength per frequency component
!
    call oswi_modulate_spectrum( oswi, verbmore )
!
!   Integrate source strength spectrum
!
    if (integrate) then
      if (oswi%hdr%fanalysis) then
        call oswi_spectrum_integrate_center_frequency ( oswi, verbmore )
        call oswi_spectrum_bandwidth ( oswi, verbmore )
      else
        call oswi_integrate_spectrum ( oswi, verbmore )
      end if
    end if
!
!   Export data to ncfile
!
    include 'liboswi_export_oswi.f90'
!
  end do
!
! ----------------------------------------------------------------------------80
!
! Clear arrays
!
! ----------------------------------------------------------------------------80
!
  call grib_2dfd_clear(ow2dfd)
  call oswi_clear(oswi)
  if (grid_convert) call oswi_clear(oswi_dummy)
  call liboswi_clear(verb)
  if (allocated(depth)) deallocate(depth)
!
! ----------------------------------------------------------------------------80
!
! Get end time
!
! ----------------------------------------------------------------------------80
!
  proc_time = omp_get_wtime() - proc_time
!
  hours   = floor( proc_time / 3600._double )
  minutes = floor( proc_time / 60._double - hours * 60._double )
  seconds = proc_time - hours * 3600._double - minutes * 60._double
!
! ----------------------------------------------------------------------------80
!
! Display terminate message
!
! ----------------------------------------------------------------------------80
!   
  if (verb) then
    write (output_unit,"(a)") ''
    write (output_unit,"(a,i2,a,i2,a,f6.3,a)") &
      'OSWI succesfully terminated. Elapsed time is ', &
      hours, ' hours ', minutes, ' minutes ', seconds, ' seconds.'
  end if
!
!*****************************************************************************80

Contains

  include 'oswi_help.f90'

End program main
