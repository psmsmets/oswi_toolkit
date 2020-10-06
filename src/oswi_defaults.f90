!
!.....swwint init
!
      call oswi_init ( oswi )
      call oswi_init ( oswi_dummy )
!
!.....default settings
!
      filename          = '%V__%C_%T_%R__%G__%Y%M%D_%H'
      overwrite         = .false.
      threads           = 1
      max_threads       = omp_get_max_threads()
      parallel          = .false.
      verb              = .false.
      quick             = .false.
      exists            = .false.
      debug             = .false.
      debug_bathymetry  = .false.
      dem%file          = ''
      in_grib           = .false.
      in_nc             = .false.
      modulate          = .true.
      integrate         = .true.
!
      grid_regular_ll   = .false.
      grid_icosahedron  = .false.
!
!.....default values
!
      nofinfs           = 0_int32
!
