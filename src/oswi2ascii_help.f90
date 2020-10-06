!*****************************************************************************80
!
!                             O S W I 2 A S C I I _ H E L P
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
!  Description:  This module contains a subroutine to print the oswi help text
!                to the stdout screen.
!
!
!*****************************************************************************80


!*****************************************************************************80
Subroutine oswi2ascii_show_help ( )
!*****************************************************************************80
!
! Subroutine oswi2ascii_show_help
!
! Description
!
!   Show oswi help text
!
!*****************************************************************************80
!
  write (output_unit,"(a)") ''
  write (output_unit,"(3a)") 'oswi2ascii ', trim(liboswi_version()), ' - Convert oswi netCDF to plain ascii'
  write (output_unit,"(a)") ''
  write (output_unit,"(a)") 'Usage: oswi2ascii nc_file [OPTIONS]'
  write (output_unit,"(a)") ''    
  write (output_unit,"(a)") 'Options and arguments (except files and paths) are not case sensitive.'    
  write (output_unit,"(a)") 'Mandatory arguments to long options are mandatory for short options too.'
  write (output_unit,"(a)") 'General:'
  write (output_unit,"(2x,2a)")'-?, --help              ','Display this help text.'
  write (output_unit,"(2x,2a)")'-a, --variable ..       ','Set variable to export.'
  write (output_unit,"(2x,2a)")'-f, --frequency .. ..   ','Specify the frequency range over which the source'
  write (output_unit,"(28x,a)")                           'strength spectrum is calculated. Both upper and lower'
  write (output_unit,"(28x,a)")                           'frequency should be provided. Use -1 to extend the'
  write (output_unit,"(28x,a)")                           'range automatically to the lower / upper limit.'
  write (output_unit,"(28x,a)")                           'Note that the input frequency is the ocean wave'
  write (output_unit,"(28x,a)")                           'frequency for --swh and the acoustic frequency for'
  write (output_unit,"(28x,a)")                           'other types (thus twice the ocean wave frequency).'
  write (output_unit,"(2x,2a)")'-p, --plain             ','Print data only.'
  write (output_unit,"(2x,2a)")'-s, --sort              ','Sort data ascending before printing.'
  write (output_unit,"(2x,2a)")'-v, --verbose           ','Verbose information and warnings'
!                                             ***************************************************80
  write (output_unit,"(a)") 'Debugging:'
  write (output_unit,"(2x,2a)")'--debug                 ','Verbose everything.'
  write (output_unit,"(2x,2a)")'--version               ','Print version number.'
!
  write (output_unit,"(a)") 'Plot output using GMT:'
  write (output_unit,"(2x,a)") 'psxy -Rg -JG-30/30/15c -Sc2p -Cswh.cpt -Xc -Yc -Bg15 -: -P > plot.ps'
!                                             ***************************************************80
  write (output_unit,"(2x,a)") ''
  stop
!
!*****************************************************************************80
End subroutine oswi2ascii_show_help
!*****************************************************************************80
