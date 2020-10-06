Program main
!*****************************************************************************80
!
!! OSWI_STACK 
!
!  Description:
!
!    Stack oswi netcdf files
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
  use liboswi
  use omp_lib
!
  implicit none
! 
! declare varibles
!
  type(type_oswi), target :: oswi, oswi_dummy
  type(type_oswi_stack), target :: oswi_stack
!
  integer(int32), parameter  :: maxinfs    = 999_int32
  integer(int32), parameter  :: infssize   = 256_int32
!
  logical         :: exists, overwrite, parallel, verb, verbmore, add_to_history, debug, &
                     quick, set_in_reference, force_regular_ll, modulate, integrate, &
                     grid_convert, grid_regular_ll, grid_icosahedron, in_grib, in_nc
  integer(int32)  :: i, inf, nofinfs, io, iargc, threads, max_threads, hours, minutes
  real(double)    :: proc_time, seconds, dummy_double
!
  logical                , dimension(maxinfs)  :: infs_mask
  integer(int32)         , dimension(maxinfs)  :: ix
  integer(int64)         , dimension(maxinfs)  :: infs_epoch
  character(len=infssize), dimension(maxinfs)  :: infs 
  character(len=infssize)                      :: inf_dummy, arg_type, arg_value, arg_value2
  character(len=infssize)                      :: prefix, suffix, ext, filename
  character(len=maxinfs)                       :: ncfile
!
! Get start time
!
  proc_time = omp_get_wtime() 
!
! Set default parameters
!
  include 'oswi_stack_defaults.f90'
!
! Get command line arguments and update output
!
  include 'oswi_stack_argc.f90'
!!  call oswi_set_output ( oswi )
!
!
! check nofinfs 
!
  if ( nofinfs.lt.1_int32 ) then
    print "(a)", 'Error: no netcdf files provided!'
  end if
!
! Parallelize
!
  if (parallel) call omp_set_num_threads( threads )
!
! Print header
!
  if (verb) then
    print "(a)", ''
    print "(a)", 'OSWI_STACK - Stack Ocean Surface Wave Interaction'
    print "(a)", ''
    print "('> ',a)", 'Options'
    print "(4x,a,l1)"        , 'Overwrite              = ', overwrite
    if (quick) print "(4x,a)", 'Quick and dirty        = T'
    print "(4x,a,i4)"        , 'OpenMP threads         = ', threads
    if (len_trim(prefix).gt.0) print "(4x,2a)", 'prefix                 = ', trim(prefix)
    if (len_trim(suffix).gt.0) print "(4x,2a)", 'suffix                 = ', trim(suffix)
    if (oswi%hdr%variance  ) print "(4x,a)" , 'variance               = T'
    if (oswi%hdr%normalize ) print "(4x,a)" , 'normalize              = T'
    if (oswi%hdr%integrate ) print "(4x,a)" , 'integrate              = T'
  end if
!     
! Evaluate input files 
!
  write (inf_dummy,"(i6)") nofinfs
!
! scan oswi netcdf files
!
  include 'liboswi_in_nc.f90'
!
  if (debug) print "('> ',a)", 'Oswint grid info'
  if (debug) call oswi_print_grid_info ( oswi%hdr%grid, "4x," )
!
! verbose more
!
  verbmore = verb.and.count(infs_mask).eq.1_int32
  if (debug) verbmore=.true.
!
! copy header from oswi to oswi_stack
!
  call oswi_copy_hdr_essentials ( oswi%hdr, oswi_stack%hdr ) 
!     
! ----------------------------------------------------------------------------80
!
! COMMON THINGS
!
! ----------------------------------------------------------------------------80
!
! set what to do when
!
  integrate=integrate.and.oswi%hdr%integrate
!
! Init grid conversion
!
  include 'liboswi_grid_transform.f90'
!     
! ----------------------------------------------------------------------------80
!
! PROCESS GRIB_FILES
!
! ----------------------------------------------------------------------------80
!
!
  if (verb) then
    print "('> ',2a)", 'Stack ', strlowcase(trim(oswi%hdr%long_name)) 
    if (oswi%hdr%swh.or.oswi%hdr%swi) then
      print "(4x,a,2(f8.5,a),i2,a)", 'wave frequency         = ', &
        oswi%hdr%f%wave(oswi%hdr%f%ixFirst), ' Hz to ', &
        oswi%hdr%f%wave(oswi%hdr%f%ixLast), ' Hz (#', oswi%hdr%f%count ,')'
    else
      print "(4x,a,2(f8.5,a),i2,a)", 'sound frequency        = ', &
        oswi%hdr%f%sound(oswi%hdr%f%ixFirst), ' Hz to ', &
        oswi%hdr%f%sound(oswi%hdr%f%ixLast), ' Hz (#', oswi%hdr%f%count ,')'
    end if
    print "(4x,2a)", 'NetCDF variable name   = ', trim(oswi%hdr%name)
    print "(4x,2a)", 'units                  = ', trim(oswi%hdr%units)
    print "(4x,2a)", 'long name              = ', trim(oswi%hdr%long_name)
  end if
!
  do inf=1_int32, nofinfs
!
!   Make (new) ncfile?
!
    include 'liboswi_ncfile.f90'
!
!   Read oswi netcdf data
!
    call oswi_read_nc_data ( &
      nc     = trim(infs(inf)), &
      dat    = oswi%dat, &
      mask   = infs_mask(inf), & 
      debug  = debug &
      )
    if (.not.infs_mask(inf)) cycle
!
!   Integrate source strength spectrum
!
    if (integrate) call oswi_integrate_spectrum( oswi, verbmore )
!
!   Stack oswi data (counter in oswi_stack!)
!
!    call oswi_stack_data( oswi_stack, oswi, verbmore )
!
  end do
!
! Convert output: dB, variance, normalize
!
!  call oswi_modify_output( oswi, verbmore )
!
! Export data to ncfile
!
  include 'liboswi_export_oswi_stack.f90'

!
! ----------------------------------------------------------------------------80
!
! Clear arrays
!
! ----------------------------------------------------------------------------80
!
  call oswi_clear(oswi)
  if (force_regular_ll) call oswi_clear(oswi_dummy)
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
    print "(a)", ''
    print "(a,i2,a,i2,a,f6.3,a)", &
      'oswi_stack succesfully terminated. Elapsed time is ', &
      hours, ' hours ', minutes, ' minutes ', seconds, ' seconds.'
  end if
!
!*****************************************************************************80

Contains

  include 'oswi_stack_help.f90'

End program main
