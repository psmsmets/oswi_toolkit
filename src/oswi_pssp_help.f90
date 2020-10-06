!*****************************************************************************80
!
!                      O S W I _ P S S P _ H E L P
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
Subroutine oswi_pssp_show_help ( )
!*****************************************************************************80
!
! Subroutine oswi_pssp_show_help
!
! Description
!
!   Show oswi help text
!
!*****************************************************************************80
!
  write (output_unit,"(a)") ''
  write (output_unit,"(3a)") 'oswi_pssp ', trim(liboswi_version()), &
    ' - Generate oswi pseudo spectra for a given location and/or area.'
  write (output_unit,"(a)") ''
  write (output_unit,"(a)") 'Usage: oswi_pssp nc_file [OPTIONS]'
  write (output_unit,"(a)") ''    
  write (output_unit,"(a)") 'Options and arguments (except files and paths) are not case sensitive.'    
  write (output_unit,"(a)") 'Mandatory arguments to long options are mandatory for short options too.'
  write (output_unit,"(a)") 'General:'
  write (output_unit,"(2x,2a)")'-?, --help              ','Display this help text.'
  write (output_unit,"(2x,2a)")'-a, --area ..           ','Specify the source area centered at -c :'
  write (output_unit,"(28x,a)")                           ' -       : a point (no area, default)'
  write (output_unit,"(28x,a)")                           ' lat/lon : a box with height lat and width lon'
  write (output_unit,"(28x,a)")                           ' radius  : a circle with radius in km'
  write (output_unit,"(28x,a)")                           ' g(lobal): use all global oswi data'
  write (output_unit,"(2x,2a)")'-c, --center ../..       ','Specify the source center location (lat/lon). If not'
  write (output_unit,"(28x,a)")                           ' provided, the pseudo spectrum location is used.'
!                                                           ***************************************************80
  write (output_unit,"(2x,2a)")'-f, --frequency .. ..   ','Specify the frequency range over which the source'
  write (output_unit,"(28x,a)")                           'strength spectrum is calculated. Both upper and lower'
  write (output_unit,"(28x,a)")                           'frequency should be provided. Use -1 to extend the'
  write (output_unit,"(28x,a)")                           'range automatically to the lower / upper limit.'
  write (output_unit,"(28x,a)")                           'Note that the input frequency is the ocean wave'
  write (output_unit,"(28x,a)")                           'frequency for --swh and the acoustic frequency for'
  write (output_unit,"(28x,a)")                           'other types (thus twice the ocean wave frequency).'  
  write (output_unit,"(2x,2a)")'-n, --filename ..       ','Set filename. Default is "%V__PSSP__%C_%T__%Y%M%D_%H".'
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
  write (output_unit,"(2x,2a)")'-p, --poi ../..          ','Specify the pseudo spectrum location (lat/lon).'
  write (output_unit,"(2x,2a)")'-t, --tloss ..           ','Transmission loss applied :'
  write (output_unit,"(28x,a)")                           ' 0       : no loss (default)'
  write (output_unit,"(28x,a)")                           ' 1       : 1/r'
  write (output_unit,"(28x,a)")                           ' 3/2     : 1/r**3/2'
  write (output_unit,"(28x,a)")                           ' 2       : 1/r**2'
  write (output_unit,"(2x,2a)")'-q, --quick             ','Quick and dirty, avoid all checks.'
! write (output_unit,"(2x,2a)")'-s, --split             ','Split data to individual arrays.'
  write (output_unit,"(2x,2a)")'-v, --verbose           ','Verbose information and warnings'
!                                                           ***************************************************80
  write (output_unit,"(a)") 'Debugging:'
  write (output_unit,"(2x,2a)")'--debug                 ','Verbose everything.'
  write (output_unit,"(2x,2a)")'--version               ','Print version number.'
!                                                           ***************************************************80
  print "(2x,a)", ''
  stop
!
!*****************************************************************************80
End subroutine oswi_pssp_show_help
!*****************************************************************************80
