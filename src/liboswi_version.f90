!*****************************************************************************80
!
!                             L I B O S W I _ V E R S I O N
!
!  Module:       /
!
!  Programmer:   Pieter S. M. Smets
!                R&D depart. of Seismology and Acoustics - Koninklijk Nederlands
!                Meteorologisch Instituut (KNMI)
!                De Bilt, The Netherlands
!
!  Date:         May 20, 2016
!
!  Language:     Fortran-90
!
!  Description:  File containing the oswi_toolkit version number, imported from
!                version.sh in the package top directory.
!
!
!*****************************************************************************80

Subroutine liboswi_package_info ( package_version, package_fullname )
!*****************************************************************************80
!
!! LIBOSWI_PACKAGE_INFO
!
!  Description:
!
!    Get liboswi package info  
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
!
!****************************
!
  implicit none
!
! Dummy variable
!
  character(len=5) , intent(out), optional  :: package_version
  character(len=25), intent(out), optional  :: package_fullname
!
! Local variables
!
  character(len=20)  :: package_name
  character(len=5)   :: oswi_version
  character(len=2)   :: oswi_major_version_number
  character(len=1)   :: oswi_minor_version_number, oswi_revision_version_number
  integer(int16)     :: oswi_major_version, oswi_minor_version, oswi_revision_version

  include '../version.sh'

  write(oswi_major_version_number,"(i2)") oswi_major_version
  write(oswi_minor_version_number,"(i1)") oswi_minor_version
  write(oswi_revision_version_number,"(i1)") oswi_revision_version

  write(oswi_version,"(a,2('.',a))") trim(adjustl(oswi_major_version_number)), &
    oswi_minor_version_number, oswi_revision_version_number

  if (present(package_version)) package_version=oswi_version
  if (present(package_fullname)) write(package_fullname,"(3a)") &
    trim(package_name), ' v', trim(oswi_version)
!
  return
!
!*****************************************************************************80
End subroutine liboswi_package_info


Function liboswi_version() result(version)
!*****************************************************************************80
!
!! LIBOSWI_VERSION
!
!  Description:
!
!    Get liboswi version number  
!
!  Modified:
!
!    20 March 2017
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!
!****************************
!
  implicit none
  character(len=5) :: version
  call liboswi_package_info(package_version=version)
!
!*****************************************************************************80
End function liboswi_version


Function liboswi_fullname() result(fullname)
!*****************************************************************************80
!
!! LIBOSWI_FULLNAME
!
!  Description:
!
!    Get liboswi full version name  
!
!  Modified:
!
!    20 March 2017
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!
!****************************
!
  implicit none
  character(len=25) :: fullname
  call liboswi_package_info(package_fullname=fullname)
!
!*****************************************************************************80
End function liboswi_fullname

