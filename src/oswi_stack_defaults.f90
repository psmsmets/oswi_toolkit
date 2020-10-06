!
!.....init oswi_stack structure
!
      call oswi_stack_init ( oswi_stack )
!
!.....default settings
!
      prefix            = ''
      suffix            = ''
      filename          = '%V__%C_%T__%G__%Y%M%D_%H'
      overwrite         = .false.
      threads           = 1
      max_threads       = omp_get_max_threads()
      parallel          = .false.
      verb              = .false.
      quick             = .false.
      exists            = .false.
      debug             = .false.
      integrate         = .true.
!
      grid_regular_ll   = .false.
      grid_icosahedron  = .false.
!
!.....default values
!
      nofinfs           = 0_int32
!
