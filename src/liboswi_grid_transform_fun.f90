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
!  Description:  liboswi set types functions and subroutines include
!
!
!*****************************************************************************80


Subroutine oswi_grid_to_regular_ll ( grid, verbose )
!*****************************************************************************80
!
!! OSWI_GRID_TO_REGULAR_LL
!
!  Description:
!
!    Convert the oswi grid content from reduced to regular ll.
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
!    Input/output, type ( type_oswi_grid ) grid, oswi grid structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid), intent(inout)  :: grid
  logical, intent(in), optional        :: verbose
!
! Local variables
!
  logical         :: verb
  integer(int32)  :: i
!
  if (grid%gridType.ne.'reduced_ll') return
!
  verb=.false.
  if (present(verbose)) verb=verbose
  if (verb) print "(a)", '> Convert reduced_ll grid to regular_ll'
!
! change longitude
!
  grid%lon%increment=grid%lat%increment
  if ((grid%lon%last-grid%lon%first+grid%lon%increment).ge.360._double) then
    grid%lon%first=0._double
    grid%lon%last=360._double
  end if
  grid%lon%size=nint((grid%lon%last-grid%lon%first)/grid%lon%increment+1_int32,int32)
  grid%lon%count=grid%lon%size
  if (allocated(grid%lon%dat).and.size(grid%lon%dat).ne.grid%lon%size) deallocate(grid%lon%dat)
  if (.not.allocated(grid%lon%dat)) allocate(grid%lon%dat(grid%lon%size))
  grid%lon%dat=(/ (i*grid%lon%increment,i=0_int32,grid%lon%size-1_int32,1_int32) /)
  if (grid%lon%reversed) then
    grid%lon%dat=grid%lon%first-grid%lon%dat
  else
    grid%lon%dat=grid%lon%first+grid%lon%dat
  end if
  if (allocated(grid%lon%mask).and.size(grid%lon%mask).ne.grid%lon%size) deallocate(grid%lon%mask)
  if (.not.allocated(grid%lon%mask)) allocate(grid%lon%mask(grid%lon%size))
  grid%lon%mask=.true.
  grid%lon%ixFirst=minloc(pack(grid%lon%dat,grid%lon%mask),1)
  grid%lon%ixLast=maxloc(pack(grid%lon%dat,grid%lon%mask),1)
!
! update grid definition
!
  grid%distinctGridValues=.true.
  grid%gridType='regular_ll'
  grid%numberOfPoints=grid%lon%count*grid%lat%count
  if (allocated(grid%pl)) deallocate(grid%pl)
!
! construct lat lon lists
!
!  call oswi_unpack_grid(grid)
!
  return
!
!*****************************************************************************80
End subroutine oswi_grid_to_regular_ll


Subroutine oswi_grid_to_icosahedron ( grid, verbose )
!*****************************************************************************80
!
!! OSWI_GRID_TO_REGULAR_LL
!
!  Description:
!
!    Convert the oswi grid content from reduced to regular ll.
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
!    Input/output, type ( type_oswi_grid ) grid, oswi grid structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid), intent(inout)  :: grid
  logical, intent(in), optional        :: verbose
!
! Local variables
!
  logical         :: verb
  character       :: str*20
  integer(int32)  :: i, factor, nof_nodes, max_nodes
  real(double)    :: edge, new_edge, edist, node_lat, node_lon
  real(double), dimension(:,:), allocatable, target  :: node_all
  real(double), dimension(:,:)             , pointer :: node_xyz
  real(double), dimension(:)               , pointer :: node_xyz1, node_xyzi
!
  if (grid%gridType.ne.'reduced_ll') return
!
  verb=.false.
  if (present(verbose)) verb=verbose
  if (verb) print "(a)", '> Convert reduced_ll grid to regular icosahedron'
!
! init
!
  factor=0
  edge=grid%earthRadius*grid%lat%increment*pi/180
  new_edge=2*pi*grid%earthRadius
!
  if (allocated(grid%pl)) deallocate(grid%pl)
  if (allocated(grid%lat%dat)) deallocate(grid%lat%dat)
  if (allocated(grid%lat%mask)) deallocate(grid%lat%mask)
  if (allocated(grid%lon%dat)) deallocate(grid%lon%dat)
  if (allocated(grid%lon%mask)) deallocate(grid%lon%mask)
!
! set vector 
!
  max_nodes=nint(grid%numberofpoints*1.05_double,int32)
  allocate(node_all(3,max_nodes))
!
! find number of nodes to fit edge
!
  do while(new_edge.gt.edge)

    factor=factor+1

    call sphere_icos_point_num ( factor, nof_nodes )
    if (nof_nodes.gt.max_nodes) stop 'oswi_grid_to_icosahedron: increase node array!'
    node_xyz=>node_all(:,1:nof_nodes)

    call sphere_icos2_points ( factor, nof_nodes, node_xyz )

    node_xyz = grid%earthRadius * node_xyz

    ! Determine the distance between adjacent nodes
    node_xyz1=>node_xyz(:,1)
    do i=2_int32,100_int32 !nof_nodes
      node_xyzi=>node_xyz(:,i)
      call sphere_distance_xyz(node_xyz1,node_xyzi,edist)
      if(edist.lt.new_edge) new_edge=edist
    enddo
  enddo
  if (verb) then
    write(str,"(i9)") nof_nodes
    print "(4x,2a,f7.3,a,i4)", trim(adjustl(str)),' nodes on the Earth separated by ',new_edge/1000., &
      ' km with factor ', factor
  end if
!
  grid%numberOfPoints=nof_nodes
  allocate(grid%lat%dat(nof_nodes),grid%lon%dat(nof_nodes))
!
  do i=1_int32,nof_nodes
    node_xyzi=>node_xyz(:,i)
    node_lat=acos(node_xyzi(3)/grid%earthRadius)
    node_lon=atan2(node_xyzi(2)/grid%earthRadius,node_xyzi(1)/grid%earthRadius)
    if(node_lat .le. pi/2.)then
      grid%lat%dat(i)=real((360./(2*pi))*( pi/2. - node_lat ),double)
    else
      grid%lat%dat(i)=real(((-360.)/(2*pi))*( node_lat - pi/2. ),double)
    endif
    if(node_lon .lt. 0.)then
      grid%lon%dat(i)=real((360./(2*pi))*( node_lon + 2*pi ),double)
    else
      grid%lon%dat(i)=real((360./(2*pi))* node_lon ,double)
    endif
  end do
!
  grid%gridType='icosahedron'
  grid%icosahedronEdge=new_edge
!
  grid%lat%reversed=.false.
  grid%lat%size=-1_int32
  grid%lat%count=grid%lat%size
  grid%lat%increment=-9999.d0
  grid%lat%ixFirst=-1_int32
  grid%lat%ixLast=-1_int32
  grid%lat%first=-90._double
  grid%lat%last=90._double
!
  grid%lon%reversed=.false.
  grid%lon%size=-1_int32
  grid%lon%count=grid%lon%size
  grid%lon%increment=-9999.d0
  grid%lon%ixFirst=-1_int32
  grid%lon%ixLast=-1_int32
  grid%lon%first=0._double
  grid%lon%last=360._double
!
! construct lat lon lists
!
  call oswi_unpack_grid(grid)
!
  return
!
!*****************************************************************************80
End subroutine oswi_grid_to_icosahedron


Subroutine transform_dem_regular_to_grib_reduced_ll ( grid, dem, reduced_ll, missingValue )
!*****************************************************************************80
!
!! TRANSFORM_DEM_REGULAR_TO_GRIB_REDUCED_LL
!
!  Description:
!
!    Tranform regular gridded dem data to reduced latlon grid by bilinear
!    interpolation.
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
!    Input, type( type_oswi_grid ) grid, oswi grid type (reduced!).
!
!    Input, type( type_dem ) dem, dem type (with regular latlon grid).
!
!    Optional input, real(single) missingValue, missing value value (default=0._single).
!
!    Output, real( single ) reduced_ll, vector with projected dem data on reduced grid.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid)  , intent(in), target  :: grid 
  type(type_dem)        , intent(in), target  :: dem 
  real(single), optional, intent(in)          :: missingValue
  real(single), dimension(grid%numberOfPoints), intent(out), target  :: reduced_ll
!
! Local variables
!
  integer(int32)           :: ilat, ilon, cnt
  integer(int32)           :: ixDemLon1, ixDemLat1, ixDemLon2, ixDemLat2
  integer(int32), pointer  :: pl
  real(single)             :: fill
  real(single), pointer    :: dat11, dat12, dat21, dat22
  real(double)             :: dlonxlat1, dlonxlat2, dlat, dlatx, dlon12lat1, dlon12lat2, &
                                & datxlat1, datxlat2
  real(double), pointer    :: lat1, lat2, lon1, lon2, reducedLat
!
  real(double), dimension(:), pointer      :: reducedLon
  real(single), dimension(:), pointer      :: red_ll
!
!  ---
!
! Check
!
  if (grid%gridType.ne.'reduced_ll') &
    stop 'Error @ transform_dem_regular_to_grib_reduced_ll : reduced_ll input grid expected!'
  if (.not.dem%lon%global.or..not.dem%lat%global) &
    stop 'Error @ transform_dem_regular_to_grib_reduced_ll : global dem grid expected!'
!
! Init
!
  fill=0._single
  if (present(missingValue)) fill=missingValue
  reduced_ll=fill
!
! Make a little tree
!
  cnt=1_int32
  do ilat=1_int32,grid%lat%size
    pl=>grid%pl(ilat)
    if (pl.eq.0_int32) cycle
    reducedLat=>grid%lat%dat(ilat)
    reducedLon=>grid%lon%dat(cnt:cnt+pl-1_int32)
    red_ll=>reduced_ll(cnt:cnt+pl-1_int32)
    cnt=cnt+pl
!
    ixDemLat1=floor((reducedLat-dem%lat%minval)/dem%lat%step)+1_int32
    if (abs(abs(reducedLat)-90._double).lt.1e-6) then
      red_ll=dem%elev%dat(1_int32,ixDemLat1)
      cycle
    end if
    ixDemLat2=ixDemLat1+1_int32
    lat1=>dem%lat%dat(ixDemLat1)
    lat2=>dem%lat%dat(ixDemLat2)
    call distance( lat1, dem%lon%dat(1_int32), lat2, dem%lon%dat(1_int32), dlat, grid%earthRadius )
!
    do ilon=1_int32,pl
      ixDemLon1=floor(lonrangef(reducedLon(ilon)-dem%lon%minval)/dem%lon%step)+1_int32
      if (ixDemLon1.lt.1_int32.or.ixDemLon1.ge.dem%lon%count) then
        if (.not.dem%lon%global) cycle
        ixDemLon2=1_int32
      else
        ixDemLon2=ixDemLon1+1_int32
      end if
!
      lon1=>dem%lon%dat(ixDemLon1)
      lon2=>dem%lon%dat(ixDemLon2)
      dat11=>dem%elev%dat(ixDemLon1,ixDemLat1)
      dat21=>dem%elev%dat(ixDemLon2,ixDemLat1)
      dat12=>dem%elev%dat(ixDemLon1,ixDemLat2)
      dat22=>dem%elev%dat(ixDemLon2,ixDemLat2)
!
      call distance( lat1, lon1, lat1, lon2, dlon12lat1, grid%earthRadius )
      call distance( lat1, lon1, lat1, reducedLon(ilon), dlonxlat1, grid%earthRadius )
      datxlat1 = dat11 + (dat21 - dat11) * dlonxlat1 / dlon12lat1
!
      call distance( lat2, lon1, lat2, lon2, dlon12lat2, grid%earthRadius )
      call distance( lat2, lon1, lat2, reducedLon(ilon), dlonxlat2, grid%earthRadius )
      datxlat2 = dat12 + (dat22 - dat12) * dlonxlat2 / dlon12lat2
!
      call distance( lat1, reducedLon(ilon), reducedLat, reducedLon(ilon), dlatx, grid%earthRadius )
      red_ll(ilon) = real( datxlat1 + (datxlat2 - datxlat1) * dlatx / dlat, kind=single )
!       
    end do
  end do
!
  return
!
!*****************************************************************************80
End subroutine transform_dem_regular_to_grib_reduced_ll


Subroutine transform_dem_regular_to_grib_regular_ll ( grid, dem, regular_ll, missingValue )
!*****************************************************************************80
!
!! TRANSFORM_DEM_REGULAR_TO_GRIB_REGULAR_LL
!
!  Description:
!
!    Tranform regular gridded dem data to regular latlon grid by bilinear
!    interpolation.
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
!    Input, type( type_oswi_grid ) grid, oswi grid type (regular!).
!
!    Input, type( type_dem ) dem, dem type (with regular latlon grid).
!
!    Optional input, real(single) missingValue, missing value value (default=0._single).
!
!    Output, real( single ) reduced_ll, vector with projected dem data on regular grid.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid)    , intent(in)              , target  :: grid 
  type(type_dem)          , intent(in)              , target  :: dem 
  real(single), dimension(grid%numberOfPoints), intent(out), target  :: regular_ll
  real(single), optional  , intent(in)                        :: missingValue
!
! Local variables
!
  integer(int32)           :: ilat, ilon, cnt
  integer(int32)           :: ixDemLon1, ixDemLat1, ixDemLon2, ixDemLat2
  real(single)             :: fill
  real(single), pointer    :: dat11, dat12, dat21, dat22
  real(double)             :: dlonxlat1, dlonxlat2, dlat, dlatx, dlon12lat1, dlon12lat2, &
                                & datxlat1, datxlat2
  real(double), pointer    :: lat1, lat2, lon1, lon2, regularLat, regularLon
  real(single), dimension(:), pointer      :: regularDat
!
!  ---
!
! Check
!
  if (grid%gridType.ne.'regular_ll') &
    stop 'Error @ transform_dem_regular_to_grib_regular_ll : regular_ll input grid expected!'
  if (.not.dem%lon%global.or..not.dem%lat%global) &
    stop 'Error @ transform_dem_regular_to_grib_reduced_ll : global dem grid expected!'
!
! Init
!
  fill=0._single
  if (present(missingValue)) fill=missingValue
  regular_ll=fill
!
! Make a little tree
!
  cnt=1_int32
  do ilat=1_int32,grid%lat%size
    regularLat=>grid%lat%dat(ilat)
    regularDat=>regular_ll(cnt:cnt+grid%lon%size-1_int32)
    cnt=cnt+grid%lon%size
!
    ixDemLat1=floor((regularLat-dem%lat%minval)/dem%lat%step)+1_int32
    if (abs(abs(regularLat)-90._double).lt.1e-6) then
      regularDat=dem%elev%dat(1_int32,ixDemLat1)
      cycle
    end if
    ixDemLat2=ixDemLat1+1_int32
    lat1=>dem%lat%dat(ixDemLat1)
    lat2=>dem%lat%dat(ixDemLat2)
    call distance( lat1, dem%lon%dat(1_int32), lat2, dem%lon%dat(1_int32), dlat, grid%earthRadius )
!
    do ilon=1_int32,grid%lon%count
      regularLon=>grid%lon%dat(ilon)
      ixDemLon1=floor(lonrangef(regularLon-dem%lon%minval)/dem%lon%step)+1_int32
      if (ixDemLon1.lt.1_int32.or.ixDemLon1.ge.dem%lon%count) then
        if (.not.dem%lon%global) cycle
        ixDemLon2=1_int32
      else
        ixDemLon2=ixDemLon1+1_int32
      end if
!
      lon1=>dem%lon%dat(ixDemLon1)
      lon2=>dem%lon%dat(ixDemLon2)
      dat11=>dem%elev%dat(ixDemLon1,ixDemLat1)
      dat21=>dem%elev%dat(ixDemLon2,ixDemLat1)
      dat12=>dem%elev%dat(ixDemLon1,ixDemLat2)
      dat22=>dem%elev%dat(ixDemLon2,ixDemLat2)
!
      call distance( lat1, lon1, lat1, lon2, dlon12lat1, grid%earthRadius )
      call distance( lat1, lon1, lat1, regularLon, dlonxlat1, grid%earthRadius )
      datxlat1 = dat11 + (dat21 - dat11) * dlonxlat1 / dlon12lat1
!
      call distance( lat2, lon1, lat2, lon2, dlon12lat2, grid%earthRadius )
      call distance( lat2, lon1, lat2, regularLon, dlonxlat2, grid%earthRadius )
      datxlat2 = dat12 + (dat22 - dat12) * dlonxlat2 / dlon12lat2
!
      call distance( lat1, regularLon, regularLat, regularLon, dlatx, grid%earthRadius )
      regularDat(ilon) = real( datxlat1 + (datxlat2 - datxlat1) * dlatx / dlat, kind=single )
!       
    end do
  end do     
!
  return
!
!*****************************************************************************80
End subroutine transform_dem_regular_to_grib_regular_ll


Subroutine transform_oswi_reduced_to_regular_ll ( reduced_ll, regular_ll, verbose )
!*****************************************************************************80
!
!! TRANSFORM_OSWI_REDUCED_TO_REGULAR_LL
!
!  Description:
!
!    Tranform data of oswi type with reduced latlon grid to a regular
!    grid by bilinear interpolation.
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
!    Input, type( type_oswi ) reduced_ll, oswi type with reduced grid.
!
!    Input/output, type( type_oswi ) regular_ll, oswi type with regular grid.
!
!
!****************************
!

  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(in)   , target    :: reduced_ll
  type(type_oswi), intent(inout), target    :: regular_ll
  logical        , intent(in)   , optional  :: verbose
!
! Local variables
!
  logical                                :: verb
  integer(int32)                         :: ilat, ilon, cnt, pnt, a, b
  real(double)                           :: dax, dab, dpl
  logical                     , pointer  :: fanalysis
  integer(int32)              , pointer  :: pl
  real(double)                , pointer  :: reducedLat, regularLon
  real(double)  , dimension(:), pointer  :: reducedLon, red_ll, red_fd, red_fc, red_fb, red_fr
  logical       , dimension(:), pointer  :: reducedMsk
!
! Init
!
  verb=.false.
  if (present(verbose)) verb=verbose
  if (verb) print "(a)", '> Interpolate reduced_ll data on regular_ll grid'
!
! Check
!
  if (reduced_ll%hdr%grid%gridType.ne.'reduced_ll') &
    stop 'Error @ transform_oswi_reduced_to_regular_ll : reduced_ll input grid expected!'
  if (regular_ll%hdr%grid%gridType.ne.'regular_ll') &
    stop 'Error @ transform_oswi_reduced_to_regular_ll : regular_ll output grid expected!'
  if (.not.reduced_ll%hdr%integrate.or..not.regular_ll%hdr%integrate) &
    stop 'Error @ transform_oswi_reduced_to_regular_ll : data should be integrated!'
!
! Initialize
!
  call liboswi_allocate_type_dat_ll( regular_ll%hdr, regular_ll%dat%ll, .true. )
  fanalysis=>reduced_ll%hdr%fanalysis
!
! copy general stuff
!
  regular_ll%dat%epoch = reduced_ll%dat%epoch
  regular_ll%dat%time  = reduced_ll%dat%time
  regular_ll%dat%file  = reduced_ll%dat%file
!
! Loop over points
!
  cnt=1_int32
  pnt=0_int32
  do ilat=1_int32,reduced_ll%hdr%grid%lat%size
    pl=>reduced_ll%hdr%grid%pl(ilat)
    if (pl.eq.0_int32) cycle
    reducedLat=>reduced_ll%hdr%grid%lat%dat(ilat)
    reducedLon=>reduced_ll%hdr%grid%lon%dat(cnt:cnt+pl-1_int32)
    reducedMsk=>reduced_ll%dat%ll%mask(cnt:cnt+pl-1_int32)
    red_ll=>reduced_ll%dat%ll%dat(cnt:cnt+pl-1_int32)
    if (fanalysis) then
      red_fd=>reduced_ll%dat%ll%fd(cnt:cnt+pl-1_int32)
      red_fc=>reduced_ll%dat%ll%fc(cnt:cnt+pl-1_int32)
      red_fb=>reduced_ll%dat%ll%fb(cnt:cnt+pl-1_int32)
      red_fr=>reduced_ll%dat%ll%fr(cnt:cnt+pl-1_int32)
    end if
    cnt=cnt+pl
! since lat(reduced) == lat(regular)
! --> only linear interpolation between longitudes!
    if (abs(abs(reducedLat)-90._double).lt.1e-6.and.reducedMsk(1_int32)) then
      do ilon=1_int32,regular_ll%hdr%grid%lon%count
        pnt=pnt+1_int32
        regular_ll%dat%ll%dat(pnt)=red_ll(1_int32)
        if (fanalysis) then
          regular_ll%dat%ll%fd(pnt)=red_fd(1_int32)
          regular_ll%dat%ll%fc(pnt)=red_fc(1_int32)
          regular_ll%dat%ll%fb(pnt)=red_fb(1_int32)
          regular_ll%dat%ll%fr(pnt)=red_fr(1_int32)
        end if
        regular_ll%dat%ll%mask(pnt)=.true.
      end do
      cycle
    end if
    dpl=pl/360._double
    do ilon=1_int32,regular_ll%hdr%grid%lon%count
      pnt=pnt+1_int32
      regularLon=>regular_ll%hdr%grid%lon%dat(ilon)
      a=floor(regularLon*dpl)+1_int32
      if (a.eq.pl) then
        b=1_int32
      elseif (a.gt.pl) then
        a=1_int32; b=2_int32
      else
        b=a+1_int32
      end if
      if (.not.reducedMsk(a)) cycle
      call distance( reducedLat, regularLon, reducedLat, reducedLon(a), &
        dax, reduced_ll%hdr%grid%earthRadius )
      if (dax.lt.0.1_double) then
        regular_ll%dat%ll%dat(pnt)=red_ll(a)
        if (fanalysis) then
          regular_ll%dat%ll%fd(pnt)=red_fd(a)
          regular_ll%dat%ll%fc(pnt)=red_fc(a)
          regular_ll%dat%ll%fb(pnt)=red_fb(a)
          regular_ll%dat%ll%fr(pnt)=red_fr(a)
        end if
        regular_ll%dat%ll%mask(pnt)=.true.
        cycle
      end if
      if (.not.reducedMsk(b)) cycle
      call distance( reducedLat, reducedLon(a), reducedLat, reducedLon(b), &
        dab, reduced_ll%hdr%grid%earthRadius )
      regular_ll%dat%ll%dat(pnt)=red_ll(a)+(red_ll(b)-red_ll(a))*dax/dab
      if (fanalysis) then
        regular_ll%dat%ll%fd(pnt)=red_fd(a)+(red_fd(b)-red_fd(a))*dax/dab
        regular_ll%dat%ll%fc(pnt)=red_fc(a)+(red_fc(b)-red_fc(a))*dax/dab
        regular_ll%dat%ll%fb(pnt)=red_fb(a)+(red_fb(b)-red_fb(a))*dax/dab
        regular_ll%dat%ll%fr(pnt)=red_fr(a)+(red_fc(b)-red_fr(a))*dax/dab
      end if
      regular_ll%dat%ll%mask(pnt)=.true.
    end do
  end do
!
  regular_ll%hdr%grid%interpolated=.true.
  regular_ll%dat%ll%maxValue=maxval(regular_ll%dat%ll%dat,regular_ll%dat%ll%mask)
!
  return
!
!*****************************************************************************80
End subroutine transform_oswi_reduced_to_regular_ll


Subroutine transform_oswi_reduced_spectrum_to_regular_ll ( reduced_ll, regular_ll, verbose )
!*****************************************************************************80
!
!! TRANSFORM_OSWI_REDUCED_SPECTRUM_TO_REGULAR_LL
!
!  Description:
!
!    Tranform spectral data of oswi type with reduced latlon grid to a regular
!    grid by bilinear interpolation.
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
!    Input, type( type_oswi ) reduced_ll, oswi type with reduced grid.
!
!    Input/output, type( type_oswi ) regular_ll, oswi type with regular grid.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(in)   , target    :: reduced_ll
  type(type_oswi), intent(inout), target    :: regular_ll
  logical        , intent(in)   , optional  :: verbose
!
! Local variables
!
  logical                                :: verb
  integer(int32)                         :: ilat, ilon, cnt, pnt, a, b, k
  real(double)                           :: dax, dab, dpl
  integer(int32)              , pointer  :: pl
  real(double)                , pointer  :: reducedLat, regularLon
  real(double), dimension(:)  , pointer  :: reducedLon
  logical     , dimension(:,:), pointer  :: reducedMsk
  real(double), dimension(:,:), pointer  :: red_ll
!
! Init
!
  verb=.false.
  if (present(verbose)) verb=verbose
  if (verb) print "('> ',a)", 'Interpolate reduced_ll spectral data on regular_ll grid'
!
! Check
!
  if (reduced_ll%hdr%grid%gridType.ne.'reduced_ll') &
    stop 'Error @ transform_oswi_reduced_to_regular_ll : reduced_ll input grid expected!'
  if (regular_ll%hdr%grid%gridType.ne.'regular_ll') &
    stop 'Error @ transform_oswi_reduced_to_regular_ll : regular_ll output grid expected!'
  if (.not.all(reduced_ll%hdr%f%mask.eqv.regular_ll%hdr%f%mask)) &
    stop 'Error @ transform_oswi_reduced_to_regular_ll : frequency vector should be identical!'
!
  call liboswi_allocate_type_dat_llf( regular_ll%hdr, regular_ll%dat%llf, .true. )
!
! copy general stuff
!
  regular_ll%dat%epoch = reduced_ll%dat%epoch
  regular_ll%dat%time  = reduced_ll%dat%time
  regular_ll%dat%file  = reduced_ll%dat%file
!
! Loop over points
!
  cnt=1_int32
  pnt=0_int32
  do ilat=1_int32,reduced_ll%hdr%grid%lat%size
    pl=>reduced_ll%hdr%grid%pl(ilat)
    if (pl.eq.0_int32) cycle
    reducedLat=>reduced_ll%hdr%grid%lat%dat(ilat)
    reducedLon=>reduced_ll%hdr%grid%lon%dat(cnt:cnt+pl-1_int32)
    reducedMsk=>reduced_ll%dat%llf%mask(cnt:cnt+pl-1_int32,:)
    red_ll=>reduced_ll%dat%llf%dat(cnt:cnt+pl-1_int32,:)
    cnt=cnt+pl
! since lat(reduced) == lat(regular)
! --> only linear interpolation between longitudes!
    if (abs(abs(reducedLat)-90._double).lt.1e-6.and.all(reducedMsk(1_int32,:))) then
      do ilon=1_int32,regular_ll%hdr%grid%lon%count
        pnt=pnt+1_int32
        regular_ll%dat%llf%dat(pnt,:)=red_ll(1_int32,:)
        regular_ll%dat%llf%mask(pnt,:)=.true.
      end do
      cycle
    end if
    dpl=pl/360._double
    do ilon=1_int32,regular_ll%hdr%grid%lon%count
      pnt=pnt+1_int32
      regularLon=>regular_ll%hdr%grid%lon%dat(ilon)
      a=floor(regularLon*dpl)+1_int32
      if (a.eq.pl) then
        b=1_int32
      elseif (a.gt.pl) then
        a=1_int32; b=2_int32
      else
        b=a+1_int32
      end if
      if (all(.not.reducedMsk(a,:))) cycle
      call distance( reducedLat, regularLon, reducedLat, reducedLon(a), &
        dax, reduced_ll%hdr%grid%earthRadius )
      if (dax.lt.0.1_double) then
        regular_ll%dat%llf%dat(pnt,:)=red_ll(a,:)
        regular_ll%dat%llf%mask(pnt,:)=.true.
        cycle
      end if
      if (all(.not.reducedMsk(b,:))) cycle
      call distance( reducedLat, reducedLon(a), reducedLat, reducedLon(b), &
        dab, reduced_ll%hdr%grid%earthRadius )
      do k=1_int32,reduced_ll%hdr%f%size
        if (.not.reducedMsk(a,k).or..not.reducedMsk(b,k)) cycle
        regular_ll%dat%llf%dat(pnt,k)=red_ll(a,k)+(red_ll(b,k)-red_ll(a,k))*dax/dab
        regular_ll%dat%llf%mask(pnt,k)=.true.
      end do
    end do
  end do     
!
  regular_ll%hdr%grid%interpolated=.true.
  regular_ll%dat%llf%maxValue=maxval(regular_ll%dat%llf%dat,regular_ll%dat%llf%mask)
!
  return
!
!*****************************************************************************80
End subroutine transform_oswi_reduced_spectrum_to_regular_ll


Subroutine transform_oswi_reduced_to_icosahedron_coefficients ( reduced_ll, icosahedron, verbose )
!*****************************************************************************80
!
!! TRANSFORM_OSWI_REDUCED_TO_ICOSAHEDRON_COEFFICIENTS
!
!  Description:
!
!    Tranform data of oswi type with reduced latlon grid to an icosahedron
!    grid by bilinear interpolation.
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
!    Input, type( type_oswi ) reduced_ll, oswi type with reduced grid.
!
!    Input/output, type( type_oswi ) icosahedron, oswi type with icosahedron grid.
!
!
!****************************
!

  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(in)   , target    :: reduced_ll
  type(type_oswi), intent(inout), target    :: icosahedron
  logical        , intent(in)   , optional  :: verbose
!
! Local variables
!
  logical                                :: verb
  integer(int32)                         :: i, ilat1, ilat2, splv
  real(double)                           :: latfirst, latlast
  real(double)  , dimension(4)  :: x, y, lat, lon
  integer(int32), dimension(:), pointer  :: iconvex
  logical                     , pointer  :: icosaMsk
  integer(int32)              , pointer  :: pl1, pl2
  integer(int32), dimension(:), pointer  :: plv
  real(double)                , pointer  :: icosaLat, icosaLon, alpha, beta
  real(double)  , dimension(:), pointer  :: reducLon1vec, reducLon2vec
!
! Init
!
  verb=.false.
  if (present(verbose)) verb=verbose
  if (verb) print "('> ',a)", 'Prepare interpolation of reduced_ll data on icosahedron grid'
!
! Check
!
  if (reduced_ll%hdr%grid%gridType.ne.'reduced_ll') &
    stop 'Error @ transform_oswi_reduced_to_icosahedron : reduced_ll input grid expected!'
  if (icosahedron%hdr%grid%gridType.ne.'icosahedron') &
    stop 'Error @ transform_oswi_reduced_to_icosahedron : icosahedron output grid expected!'
!
! Initialize
!
  if (allocated(icosa_iconvex).and.(size(icosa_iconvex,2).ne.icosahedron%hdr%grid%numberOfPoints.or. &
    size(icosa_iconvex,1).ne.4)) deallocate(icosa_iconvex)
  if (.not.allocated(icosa_iconvex)) allocate(icosa_iconvex(4,icosahedron%hdr%grid%numberOfPoints))
  icosa_iconvex=0_int32
!
  if (allocated(icosa_mask).and.size(icosa_mask).ne.icosahedron%hdr%grid%numberOfPoints) &
    deallocate(icosa_mask)
  if (.not.allocated(icosa_mask)) allocate(icosa_mask(icosahedron%hdr%grid%numberOfPoints))
  icosa_mask=.false.
!
  if (allocated(icosa_alpha).and.size(icosa_alpha).ne.icosahedron%hdr%grid%numberOfPoints) &
    deallocate(icosa_alpha)
  if (.not.allocated(icosa_alpha)) allocate(icosa_alpha(icosahedron%hdr%grid%numberOfPoints))
  icosa_alpha=0._double
!
  if (allocated(icosa_beta).and.size(icosa_beta).ne.icosahedron%hdr%grid%numberOfPoints) &
    deallocate(icosa_beta)
  if (.not.allocated(icosa_beta)) allocate(icosa_beta(icosahedron%hdr%grid%numberOfPoints))
  icosa_beta=0._double
!
! set first/last latitude
!
  if (allocated(reduced_ll%hdr%grid%lat%mask)) then
    latfirst = reduced_ll%hdr%grid%lat%dat(reduced_ll%hdr%grid%lat%ixFirst)
    latlast  = reduced_ll%hdr%grid%lat%dat(reduced_ll%hdr%grid%lat%ixLast)
  else
    latfirst = reduced_ll%hdr%grid%lat%first
    latlast  = reduced_ll%hdr%grid%lat%last
  end if
!
! Loop over points
!
  do i=1_int32,icosahedron%hdr%grid%numberOfPoints
!
!   check if triangle is within latitude range 
!
    icosaLat=>icosahedron%hdr%grid%lat%dat(i)
    if (icosaLat.gt.latfirst.or.icosaLat.lt.latlast) cycle
!
!   set pointers
!
    icosaLon=>icosahedron%hdr%grid%lon%dat(i)
    icosaMsk=>icosa_mask(i)
    alpha=>icosa_alpha(i)
    beta=>icosa_beta(i)
    iconvex=>icosa_iconvex(:,i)
!
    icosaMsk=.true.
!
!   get latitude index
!
    ilat1=floor((reduced_ll%hdr%grid%lat%first-icosaLat)/reduced_ll%hdr%grid%lat%increment+1_int32,int32)
    ilat2=ilat1+1_int32
!
!   point to number of longitude vector (and check again)
!
    pl1=>reduced_ll%hdr%grid%pl(ilat1)
    pl2=>reduced_ll%hdr%grid%pl(ilat2)
    if (pl1.eq.0_int32.or.pl2.eq.0_int32) cycle
!
!   get longitude indexes from two consecutive latitude circles
!
    plv=>reduced_ll%hdr%grid%pl(1:ilat1)
    splv=sum(plv)
!
!   longitudes for lat1
!
    reducLon1vec=>reduced_ll%hdr%grid%lon%dat(splv-pl1+1:splv)
    iconvex(1)=minloc(abs(reducLon1vec-icosaLon),1,reducLon1vec-icosaLon.lt.1.d-8)
    iconvex(2)=iconvex(1)+1_int32
    if (iconvex(2).gt.pl1) iconvex(2)=1_int32
!
!   longitudes for lat2
!
    reducLon2vec=>reduced_ll%hdr%grid%lon%dat(splv+1:splv+pl2)
    iconvex(3)=minloc(abs(reducLon2vec-icosaLon),1,reducLon2vec-icosaLon.lt.1.d-8)
    iconvex(4)=iconvex(3)+1_int32
    if (iconvex(4).gt.pl2) iconvex(4)=1_int32
!
!   pointer index to global index
!
    iconvex=iconvex+splv
    iconvex(1:2)=iconvex(1:2)-pl1
!
!   set coordinate vectors
!
    lat(1:2)=reduced_ll%hdr%grid%lat%dat(ilat1)
    lat(3:4)=reduced_ll%hdr%grid%lat%dat(ilat2)
    lon=reduced_ll%hdr%grid%lon%dat(iconvex)
!
!   project to local cartesian (plane)
!
    call orthographic_projection_vec( lat, lon, icosaLat, icosaLon, x, y, reduced_ll%hdr%grid%EarthRadius )
!
!   bilinear interpolation of convex tetragon
!
    call ctbip_coef( x, y, 0.d0, 0.d0, alpha, beta  )
!
  end do
!
  return
!
!*****************************************************************************80
End subroutine transform_oswi_reduced_to_icosahedron_coefficients


Subroutine transform_oswi_reduced_to_icosahedron ( reduced_ll, icosahedron, verbose )
!*****************************************************************************80
!
!! TRANSFORM_OSWI_REDUCED_TO_ICOSAHEDRON
!
!  Description:
!
!    Tranform data of oswi type with reduced latlon grid to an icosahedron
!    grid by bilinear interpolation.
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
!    Input, type( type_oswi ) reduced_ll, oswi type with reduced grid.
!
!    Input/output, type( type_oswi ) icosahedron, oswi type with icosahedron grid.
!
!
!****************************
!

  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(in)   , target    :: reduced_ll
  type(type_oswi), intent(inout), target    :: icosahedron
  logical        , intent(in)   , optional  :: verbose
!
! Local variables
!
  logical                                :: verb
  integer(int32)                         :: i
  logical       , dimension(4)           :: msk 
  real(double)  , dimension(4)           :: z
  integer(int32), dimension(:), pointer  :: iconvex
  logical                     , pointer  :: fanalysis, icosaMsk
  real(double)                , pointer  :: icosaDat, alpha, beta
!
! Init
!
  verb=.false.
  if (present(verbose)) verb=verbose
  if (verb) print "('> ',a)", 'Interpolate reduced_ll data on icosahedron grid'
!
! Check
!
  if (reduced_ll%hdr%grid%gridType.ne.'reduced_ll') &
    stop 'Error @ transform_oswi_reduced_to_icosahedron : reduced_ll input grid expected!'
  if (icosahedron%hdr%grid%gridType.ne.'icosahedron') &
    stop 'Error @ transform_oswi_reduced_to_icosahedron : icosahedron output grid expected!'
  if (.not.reduced_ll%hdr%integrate.or..not.icosahedron%hdr%integrate) &
    stop 'Error @ transform_oswi_reduced_to_icosahedron : data should be integrated!'
!
  if (.not.allocated(icosa_mask).or..not.allocated(icosa_iconvex).or.&
    .not.allocated(icosa_alpha).or..not.allocated(icosa_beta)) &
    stop 'Error @ transform_oswi_reduced_to_icosahedron : coefficients not set'
!
! Initialize
!
  call liboswi_allocate_type_dat_ll( icosahedron%hdr, icosahedron%dat%ll, .true. )
  fanalysis=>reduced_ll%hdr%fanalysis
!
! copy general stuff
!
  icosahedron%dat%epoch = reduced_ll%dat%epoch
  icosahedron%dat%time  = reduced_ll%dat%time
  icosahedron%dat%file  = reduced_ll%dat%file
!
! Loop over points
!
  do i=1_int32,icosahedron%hdr%grid%numberOfPoints
!
!   check if triangle is within latitude range 
!
    if (.not.icosa_mask(i)) cycle
!
!   set pointers
!
    icosaDat=>icosahedron%dat%ll%dat(i)
    icosaMsk=>icosahedron%dat%ll%mask(i)
!
!   pointer index to global index
!
    iconvex=>icosa_iconvex(:,i)
    alpha=>icosa_alpha(i)
    beta=>icosa_beta(i)
!
!   set mask vector
!
    msk = reduced_ll%dat%ll%mask(iconvex)
    if (count(msk).eq.0_int32) cycle
!
!   bilinear interpolation of convex tetragon
!
    z=reduced_ll%dat%ll%dat(iconvex)
    call eval_ctbip_coef( z, alpha, beta, icosaDat, msk, icosaMsk  )
!
!   extra interpolation?
!
    if (fanalysis.and.icosaMsk) then
      z=reduced_ll%dat%ll%fd(iconvex)
      call eval_ctbip_coef( z, alpha, beta, icosahedron%dat%ll%fd(i) )
      z=reduced_ll%dat%ll%fc(iconvex)
      call eval_ctbip_coef( z, alpha, beta, icosahedron%dat%ll%fc(i) )
      z=reduced_ll%dat%ll%fb(iconvex)
      call eval_ctbip_coef( z, alpha, beta, icosahedron%dat%ll%fb(i) )
      z=reduced_ll%dat%ll%fr(iconvex)
      call eval_ctbip_coef( z, alpha, beta, icosahedron%dat%ll%fr(i) )
    end if
!
  end do
!
  icosahedron%hdr%grid%interpolated=.true.
  icosahedron%dat%ll%maxValue=maxval(icosahedron%dat%ll%dat,mask=icosahedron%dat%ll%mask)
!
  return
!
!*****************************************************************************80
End subroutine transform_oswi_reduced_to_icosahedron


Subroutine transform_oswi_reduced_spectrum_to_icosahedron ( reduced_ll, icosahedron, verbose )
!*****************************************************************************80
!
!! TRANSFORM_OSWI_REDUCED_SPECTRUM_TO_ICOSAHEDRON
!
!  Description:
!
!    Tranform data of oswi type with reduced latlon grid to an icosahedron
!    grid by bilinear interpolation.
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
!    Input, type( type_oswi ) reduced_ll, oswi type with reduced grid.
!
!    Input/output, type( type_oswi ) icosahedron, oswi type with icosahedron grid.
!
!
!****************************
!

  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(in)   , target    :: reduced_ll
  type(type_oswi), intent(inout), target    :: icosahedron
  logical        , intent(in)   , optional  :: verbose
!
! Local variables
!
  logical                      :: verb
  integer(int32)               :: i, j
  logical       , dimension(4) :: msk 
  real(double)  , dimension(4) :: z
  integer(int32), dimension(:), pointer  :: iconvex
  logical       , dimension(:), pointer  :: icosaMsk
  real(double)  , dimension(:), pointer  :: icosaDat
  real(double)                , pointer  :: alpha, beta
!
! Init
!
  verb=.false.
  if (present(verbose)) verb=verbose
  if (verb) print "('> ',a)", 'Interpolate reduced_ll spectral data on icosahedron grid'
!
! Check
!
  if (reduced_ll%hdr%grid%gridType.ne.'reduced_ll') &
    stop 'Error @ transform_oswi_reduced_spectrum_to_icosahedron : reduced_ll input grid expected!'
  if (icosahedron%hdr%grid%gridType.ne.'icosahedron') &
    stop 'Error @ transform_oswi_reduced_spectrum_to_icosahedron : icosahedron output grid expected!'
  if (.not.all(reduced_ll%hdr%f%mask.eqv.icosahedron%hdr%f%mask)) &
    stop 'Error @ transform_oswi_reduced_spectrum_to_icosahedron : frequency vector should be identical!'
!
  if (.not.allocated(icosa_mask).or..not.allocated(icosa_iconvex).or.&
    .not.allocated(icosa_alpha).or..not.allocated(icosa_beta)) &
    stop 'Error @ transform_oswi_reduced_spectrum_to_icosahedron : coefficients not set'
!
! Initialize

  call liboswi_allocate_type_dat_llf( icosahedron%hdr, icosahedron%dat%llf, .true. )
  icosahedron%dat%llf%mask=.false.
!
! copy general stuff
!
  icosahedron%dat%epoch = reduced_ll%dat%epoch
  icosahedron%dat%time  = reduced_ll%dat%time
  icosahedron%dat%file  = reduced_ll%dat%file
!
! Loop over points
!
  do i=1_int32,icosahedron%hdr%grid%numberOfPoints
!
!   check if triangle is within latitude range 
!
    if (.not.icosa_mask(i)) cycle
!
!   set pointers
!
    icosaMsk=>icosahedron%dat%llf%mask(i,:)
    icosaDat=>icosahedron%dat%llf%dat(i,:)
!
!   pointer index to global index
!
    iconvex=>icosa_iconvex(:,i)
    alpha=>icosa_alpha(i)
    beta=>icosa_beta(i)
!
!   bilinear interpolation of convex tetragon
!
    do j=icosahedron%hdr%f%ixFirst,icosahedron%hdr%f%ixLast
      msk = reduced_ll%dat%llf%mask(iconvex,j)
      if (count(msk).eq.0_int32) cycle
      z=reduced_ll%dat%llf%dat(iconvex,j)
      call eval_ctbip_coef( z, alpha, beta, icosaDat(j), msk, icosaMsk(j)  )
    end do
!
  end do
!
  icosahedron%hdr%grid%interpolated=.true.
  icosahedron%dat%llf%maxValue=maxval(icosahedron%dat%llf%dat,icosahedron%dat%llf%mask)
!
  return
!
!*****************************************************************************80
End subroutine transform_oswi_reduced_spectrum_to_icosahedron
