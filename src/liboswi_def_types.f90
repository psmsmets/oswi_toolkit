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
!  Date:         June 9, 2017
!
!  Language:     Fortran-90
!
!  Description:  liboswi types definitions include
!
!
!*****************************************************************************80
!
! Define type oswi input
!
  type type_oswi_env_var
    character(len=len_att)    :: name, long_name, standard_name, units
    real(double)              :: value
  end type type_oswi_env_var
!
  type type_oswi_env
    type(type_oswi_env_var) :: rho_air, c_air, rho_sea, c_sea, rho_bed, c_bed
  end type type_oswi_env
!
  type type_oswi_osw
    logical                   :: hasselmann, directionFullCircle, interpolated
    character(len=len_att)    :: shortName, units, centre, &
                                 dataClass, dataType, dataStream, gridType
    integer(int32)            :: paramId, experimentVersionNumber, editionNumber, directions, &
                                 frequencies, forecastStep, ensembleNumber
    real(double)              :: directionIncrement, firstFrequency, frequencyScalar
  end type type_oswi_osw
!
  type type_oswi_dem
    logical                   :: defined, node_offset, useNativeRes, interpolated
    character(len=len_att)    :: resampleMethod, units
    character(len=len_att_gl) :: title, institution, source, history, references
  end type type_oswi_dem
!
! Define type oswi grid
!
  type type_oswi_grid_ll
    logical                    :: reversed
    integer(int32)             :: size, count, ixFirst, ixLast
    real(double)               :: first, last, increment, range
    real(double), dimension(:), allocatable  :: dat, dat_unpacked
    logical     , dimension(:), allocatable  :: mask
  end type type_oswi_grid_ll
!
  type type_oswi_grid
    character(len=len_att)     :: gridType
    logical                    :: distinctGridValues, earthIsOblate, interpolated 
    integer(int32)             :: numberOfPoints, earthRadius
    real(double)               :: icosahedronEdge 
    integer(int32), dimension(:), allocatable  :: pl !  number of points along a full parallel
!   logical       , dimension(:), allocatable  :: mask
    type(type_oswi_grid_ll)  :: lon, lat
  end type type_oswi_grid
!
! Define type oswi frequency
!
  type type_oswi_f
    integer(int32)             :: size, count, ixFirst, ixLast, ixSel
    real(double)               :: first, last
    real(double), dimension(:), allocatable :: sound, wave 
    real(double), dimension(:), pointer     :: dat 
    logical     , dimension(:), allocatable :: mask
  end type type_oswi_f
!
! Define type oswi
!
  type type_oswi_hdr
    type(type_oswi_env)       :: env
    type(type_oswi_osw)       :: osw
    type(type_oswi_dem)       :: dem 
    type(type_oswi_grid)      :: grid
    type(type_oswi_f)         :: f
    logical                   :: global, clip, integrate, fanalysis, normalize, variance, dB, Pa, lg, debug
    logical                   :: air, sea, bed, swh, swi, sfd
    character(len=3)          :: type
    character(len=len_att)    :: name, long_name, standard_name, units
    character(len=len_att_gl) :: history
    integer(int32)            :: id, missingValue, referencePressure
  end type type_oswi_hdr
!
  type type_oswi_dat_ll
    real(double)                              :: maxValue
    logical      , dimension(:), allocatable  :: mask
    integer(int8), dimension(:), allocatable  :: ifd
    real(double) , dimension(:), allocatable  :: dat, fc, fd, fb, fr
  end type type_oswi_dat_ll
!
  type type_oswi_dat_llf
    real(double)                                :: maxValue
    logical      , dimension(:,:), allocatable  :: mask
    real(double) , dimension(:,:), allocatable  :: dat
  end type type_oswi_dat_llf
!
  type type_oswi_dat
!   logical                   :: spectrum, fanalysis
    integer(int64)            :: epoch
    character(len=time_len)   :: time
    character(len=len_att_gl) :: file
    type(type_oswi_dat_ll)    :: ll
    type(type_oswi_dat_llf)   :: llf
  end type type_oswi_dat
!
  type type_oswi
    type(type_oswi_hdr)       :: hdr
    type(type_oswi_dat)       :: dat
  end type type_oswi
!
! Define type oswi_stack
!
  type type_oswi_stack_dat_var
    real(double)  , dimension(:)    , allocatable  :: ll, fc, fd, fb, fr
      ! c=center, d=dominant, b=bandwidth, r=rms
    real(double)  , dimension(:,:)  , allocatable  :: llf
  end type type_oswi_stack_dat_var
!
  type type_oswi_stack_dat
    logical                   :: ci ! move to header + two weight options (none, amplitude, energy -> moment)
    integer(int32)            :: numberOfFiles
    real(double)              :: maxValue, significance_level
    integer(int64)            :: t0, t1
    integer(int64), dimension(:)  , allocatable  :: epoch
    logical       , dimension(:)  , allocatable  :: mask
    real(double)  , dimension(:)  , pointer      :: f
    real(double)  , dimension(:)  , allocatable  :: w
    real(double)  , dimension(:,:), allocatable  :: wf
    type(type_oswi_stack_dat_var)  :: mu, std, ci_lb, ci_ub
  end type type_oswi_stack_dat
!
  type type_oswi_stack
    type(type_oswi_hdr)       :: hdr
    type(type_oswi_stack_dat) :: dat
  end type type_oswi_stack
!
! Define type oswi_list
!
  type type_oswi_list
    integer(int32)            :: numberOfFiles
    type(type_oswi_hdr)       :: hdr
    type(type_oswi_dat), dimension(:), allocatable  :: dat
  end type type_oswi_list
!
! Define type oswi_pssp
!
! Reference viewpoint (receiver)
  type type_oswi_pssp_rvp
    logical                     :: integrate_area, integrate_spec
    real(double)                :: lat, lon
  end type type_oswi_pssp_rvp
! Source area
  type type_oswi_pssp_sar
    logical                   :: setradius, setrange, setnearest, integrate, global
    integer(int32)            :: numberOfPoints
    real(double)              :: latCen, lonCen, radius, area
    real(double)              :: latMin, latMax, lonMin, lonMax
    logical       , dimension(:), allocatable  :: mask
    integer(int32), dimension(:), allocatable  :: ix
    real(double)  , dimension(:), allocatable  :: lat, lon
  end type type_oswi_pssp_sar
! Direction and distance source - receiver
  type type_oswi_pssp_dir
    logical                   :: discrete, integrate_dir ! mean, std, int
    integer(int32)            :: size
    real(double)              :: first, last, increment, range
    integer(int32), dimension(:), allocatable  :: ix
    real(double)  , dimension(:), allocatable  :: bearing, distance
  end type type_oswi_pssp_dir
! Source propagation
  type type_oswi_pssp_tl
    logical                   :: spherical, cylindrical, practical
    real(double)              :: tl_coeff
    real(double)  , dimension(:), allocatable  :: tl
  end type type_oswi_pssp_tl
! Data
  type type_oswi_pssp_dat
    integer(int32)            :: numberOfFiles
    real(double)              :: maxValue
    integer(int64), dimension(:)    , allocatable  :: epoch
    logical       , dimension(:)    , allocatable  :: mask
    real(double)  , dimension(:,:,:), allocatable  :: tfd, mu, std
    real(double)  , dimension(:,:)  , pointer      :: tf
  end type type_oswi_pssp_dat
! Joined type
  type type_oswi_pssp
    logical                   :: poi, directional, integrate
    type(type_oswi_hdr)       :: hdr
    type(type_oswi_pssp_rvp)  :: refPoint
    type(type_oswi_pssp_sar)  :: sourceArea
    type(type_oswi_pssp_dir)  :: direction
    type(type_oswi_pssp_tl)   :: tloss
    type(type_oswi_pssp_dat)  :: dat
  end type type_oswi_pssp
