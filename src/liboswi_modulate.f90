!*****************************************************************************80
!
!                                 L I B O S W I
!
!  Module:       LIBOSWI (include)
!
!  Programmer:   Pieter S. M. Smets
!              R&DSA depart. of Seismology and Acoustics
!              Koninklijk Nederlands Meteorologisch Instituut (KNMI)
!              De Bilt, The Netherlands
!
!  Date:         May 20, 2016
!
!  Language:     Fortran-90
!
!  Description:  liboswi initialize modulation coefficients 
!
!
!*****************************************************************************80
!
!..examine input files list
!
  if (modulate) then
!
!   Calculate intensity modulation by frequency (and depth)
!
    if (oswi%hdr%dem%defined) then
!
      if (len_trim(dem%file).eq.0_int32) then
        call get_environment_variable(name='OSWI_DEM_FILES', value=dem%file, trim_name=.true.,status=io)
        if (debug) print "(2a)", '> Get enviroment variable $OSWI_DEM_FILES=', trim(dem%file)
        inquire (file=trim(dem%file), exist=exists )
        if (.not.exists) then
          print "(a)", 'Environment variable $OSWI_DEM_FILES points to a non-existing directory!'
          print "(a)", '  $OSWI_DEM_FILES = ', trim(dem%file)
          stop
        end if
!
        write (dem%file,"(2a,f5.3,a)") trim(dem%file), '/oswi_dem_bath_', oswi%hdr%grid%lat%increment, '.nc'
        if (debug) print "(3a)", '> Check if dem file "', trim(dem%file), '" exists'
        inquire (file=trim(dem%file), exist=exists )
        if (.not.exists) then
          print "(3a)", 'Required bathymetry file "', trim(dem%file), '" not found in $OSWI_DEM_FILES!'
          stop
        end if
!!        call get_environment_variable(name='OSWI_DEM_FILES', value=dem%file, trim_name=.true.,status=io)
!!        if (len_trim(dem%file).eq.0) stop 'No $OSWI_DEM_FILES environment variable defined!'
      end if
!
      if (verb) print "('> ',2a)", 'Import bathymetry from ', trim(dem%file)
        call import_dem( &
          file=trim(dem%file), &
          dem=dem, &
          verbose=.false., &
          deallocate_dims=.false., &
          flip=.true., &
          clip=.true., &
          SIunits=.true. &
        )
        allocate(depth(oswi%hdr%grid%numberOfPoints))
!
!     Match dem and oswi grid
!
      if ( lonrangef(abs(dem%lon%minval-oswi%hdr%grid%lon%first)).gt.0._double .or. &
        lonrangef(abs(dem%lon%maxval-oswi%hdr%grid%lon%last)).gt.0._double .or. &
        abs(dem%lon%step-oswi%hdr%grid%lon%increment).gt.0._double .or. &
        abs(dem%lat%minval-oswi%hdr%grid%lat%last).gt.0._double .or. &
        abs(dem%lat%maxval-oswi%hdr%grid%lat%first).gt.0._double .or. &
        abs(dem%lat%step-oswi%hdr%grid%lat%increment).gt.0._double  ) then
        if (verb) print "('> ',a)", 'Resample bathymetry'
        oswi%hdr%dem%interpolated=.true.
        if (oswi%hdr%grid%gridType.eq.'regular_ll') then
          select case (strlowcase(oswi%hdr%dem%resampleMethod))
            case ('block-mean','block','mean')
              call blocksample_dem ( &
                dem=dem, &
                range_lon=(/oswi%hdr%grid%lon%first,oswi%hdr%grid%lon%last/), &
                range_lat=(/oswi%hdr%grid%lat%last,oswi%hdr%grid%lat%first/), &
                step=(/oswi%hdr%grid%lon%increment, oswi%hdr%grid%lat%increment/), &
                method='mean' &
                )
              depth = reshape(dem%elev%dat(:,dem%lat%count:1_int32:-1_int32),(/ow2dfd%grid%numberOfPoints/))
              oswi%hdr%dem%resampleMethod = 'block-mean'
            case default
              oswi%hdr%dem%resampleMethod = 'bilinear'
              call transform_dem_regular_to_grib_regular_ll ( grid=oswi%hdr%grid, dem=dem, regular_ll=depth )
          end select
        else
          oswi%hdr%dem%resampleMethod = 'bilinear'
          call transform_dem_regular_to_grib_reduced_ll ( grid=oswi%hdr%grid, dem=dem, reduced_ll=depth )
        end if
      else
        oswi%hdr%dem%interpolated=.false.
        oswi%hdr%dem%resampleMethod = ''
        depth = reshape(dem%elev%dat(:,dem%lat%count:1_int32:-1_int32),(/oswi%hdr%grid%numberOfPoints/))
      end if
!
      if (.not.debug_bathymetry) then
        if (verb) print "('> ',a)", 'Set amplitude modulation coefficients (finite ocean depth)'
        call set_modulation_coefficients_finite_ocean_depth( oswi, depth )
      else
        if (.not.allocated(oswi%dat%ll%dat)) allocate(oswi%dat%ll%dat(oswi%hdr%grid%numberOfPoints))
        oswi%dat%ll%dat=real(depth,double)
      end if
      call deallocate_dem( dem )
!
    else
!
      if (verb) print "('> ',a)", 'Set amplitude modulation coefficients (infinite ocean depth)'
      call set_modulation_coefficients_infinite_ocean_depth( oswi )
!
    end if
!
  end if
