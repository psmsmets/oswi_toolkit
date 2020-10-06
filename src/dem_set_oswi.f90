Subroutine oswi_set_dem ( oswi, dem )
!*****************************************************************************80
!
!! OSWI_SET_DEM
!
!  Description:
!
!    Set oswi bathymetry type. 
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
!    Input/output, type ( type_oswi_dem ) oswi, oswi dem type.
!
!    Input, type ( type_dem ), dem, dem type
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_dem), intent(inout)  :: oswi
  type(type_dem)     , intent(in   )  :: dem
!
! ----
!
  oswi%defined = .true.
  oswi%node_offset = dem%attributes%node_offset
  oswi%units = dem%elev%units
  oswi%title = dem%attributes%title
  oswi%institution = dem%attributes%institution
  oswi%source = dem%attributes%source
  oswi%history = dem%attributes%history
  oswi%references = dem%attributes%references
!
  return
!
!*****************************************************************************80
End subroutine oswi_set_dem
