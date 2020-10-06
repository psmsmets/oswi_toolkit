!*****************************************************************************80
!
!                                   L I B O S W I
!
!  Module:       /
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
!  Description:  liboswi prepare grid conversion
!
!
!*****************************************************************************80
!
! init grid transform 
!
  grid_convert=.false.
  if (grid_regular_ll)  grid_convert=oswi%hdr%grid%gridType.eq.'reduced_ll'.and.grid_regular_ll
  if (grid_icosahedron) grid_convert=oswi%hdr%grid%gridType.eq.'reduced_ll'.and.grid_icosahedron
!
  if (grid_convert) then
    call oswi_copy( oswi, oswi_dummy )
    if (grid_regular_ll) then
      call oswi_grid_to_regular_ll( oswi_dummy%hdr%grid, verb )
    elseif (grid_icosahedron) then
      call oswi_grid_to_icosahedron( oswi_dummy%hdr%grid, verb )
      call transform_oswi_reduced_to_icosahedron_coefficients ( oswi, oswi_dummy, verb )
    end if
    if (debug) call oswi_print_grid_info ( oswi_dummy%hdr%grid, "4x," )
    if (     oswi%hdr%integrate) &
      allocate( oswi_dummy%dat%ll%dat(oswi_dummy%hdr%grid%numberOfPoints) )
    if (.not.oswi%hdr%integrate) &
      allocate( oswi_dummy%dat%llf%dat(oswi_dummy%hdr%grid%numberOfPoints,oswi_dummy%hdr%f%size))
  end if
