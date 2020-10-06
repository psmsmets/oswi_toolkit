!*****************************************************************************80
!
!                           O S W I 2 B I N _ H E L P
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
Subroutine oswi2bin_show_help ( )
!*****************************************************************************80
!
! Subroutine oswi2bin_show_help
!
! Description
!
!   Show oswi help text
!
!*****************************************************************************80
!
  write (output_unit,"(a)") ''
  write (output_unit,"(3a)") 'oswi2bin ', trim(liboswi_version()), ' - Convert oswi netCDF to plain ascii'
  write (output_unit,"(a)") ''
  write (output_unit,"(a)") 'Usage: oswi2bin nc_file [OPTIONS]'
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
  write (output_unit,"(2x,2a)")'-n, --filename ..       ','Set filename. Default is "%V__%T_%Y__%G__%Y%M%D_%H".'
  write (output_unit,"(28x,a)")                           'Filename variables are: '
  write (output_unit,"(28x,a)")                           ' %V  = output variable type'
  write (output_unit,"(28x,a)")                           ' %A  = output variable standard name'
  write (output_unit,"(28x,a)")                           ' %G  = output grid type'
  write (output_unit,"(28x,a)")                           ' %C  = 2dfd data class'
  write (output_unit,"(28x,a)")                           ' %T  = 2dfd data type'
  write (output_unit,"(28x,a)")                           ' %Y  = year'
  write (output_unit,"(28x,a)")                           ' %M  = month'
  write (output_unit,"(28x,a)")                           ' %D  = day of month'
  write (output_unit,"(28x,a)")                           ' %O  = day of year'
  write (output_unit,"(28x,a)")                           ' %H  = hour'
  write (output_unit,"(28x,a)")                           ' %S  = forecast step'
  write (output_unit,"(28x,a)")                           ' %N  = ensemble number'
  write (output_unit,"(28x,a)")                           ' %F  = selected frequency'
  write (output_unit,"(28x,a)")                           ' %F0 = first frequency'
  write (output_unit,"(28x,a)")                           ' %F1 = last frequency'
  write (output_unit,"(2x,2a)")'-q, --quick             ','Quick and dirty, avoid all checks.'
  write (output_unit,"(2x,2a)")'-s, --split             ','Split data to individual arrays.'
  write (output_unit,"(2x,2a)")'-v, --verbose           ','Verbose information and warnings'
!                                             ***************************************************80
  write (output_unit,"(a)") 'Debugging:'
  write (output_unit,"(2x,2a)")'--debug                 ','Verbose everything.'
  write (output_unit,"(2x,2a)")'--version               ','Print version number.'
!                                             ***************************************************80
  write (output_unit,"(2x,a)") ''
  stop
!
!*****************************************************************************80
End subroutine oswi2bin_show_help
!*****************************************************************************80
