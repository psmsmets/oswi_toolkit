!*****************************************************************************80
!
!                          O S W I _ S T A C K _ H E L P
!
!  Module:       /
!
!  Programmer:   Pieter S. M. Smets
!                R&D depart. of Seismology and Acoustics - Koninklijk Nederlands
!                Meteorologisch Instituut (KNMI)
!                De Bilt, The Netherlands
!
!  Date:         June 20, 2017
!
!  Language:     Fortran-90
!
!  Description:  This module contains a subroutine to print the oswi stack help
!                text to the stdout screen.
!
!
!*****************************************************************************80


!*****************************************************************************80
Subroutine oswi_stack_show_help ( )
!*****************************************************************************80
!
! Subroutine oswi_stack_show_help
!
! Description
!
!   Show oswi help text
!
!*****************************************************************************80
!
  print "(a)", ''
  print "(3a)", 'oswi_stack ', trim(liboswi_version()), ' - Ocean Surface Wave Interaction Stack'
  print "(a)", ''
  print "(a)", 'Usage: oswi_stack nc_files [OPTIONS]'
  print "(a)", ''    
  print "(a)", 'Options and arguments (except files and paths) are not case sensitive.'    
  print "(a)", 'Mandatory arguments to long options are mandatory for short options too.'
  print "(a)", 'General:'
  print "(2x,2a)",'-?, --help              ','Display this help text.'
  print "(2x,2a)",'-q, --quick             ','Quick and dirty, avoid all checks.'
  print "(2x,2a)",'-v, --verbose           ','Verbose information and warnings'
  print "(2x,2a)",'-n, --silent            ','No verbose.'
  print "(2x,2a)",'-p, --prefix ..         ','Add a prefix to the output file name.'
  print "(2x,2a)",'-u, --suffix ..         ','Add a suffix to the output file name.'
  print "(2x,2a)",'-o, --overwrite         ','Overwrite existing output files, by default not'
  print "(28x,a)"                           ,'allowed.'
  print "(2x,2a)",'-t, --threads [..]      ','Max number of openmp processing threads. Default = 1.'
  print "(a)", 'Input:'
  print "(2x,2a)",'-f, --frequency .. ..   ','Specify the frequency range over which the source'
  print "(28x,a)"                           ,'strength spectrum is calculated. Both upper and lower'
  print "(28x,a)"                           ,'frequency should be provided. Use -1 to extend the'
  print "(28x,a)"                           ,'range automatically to the lower / upper limit.'
  print "(28x,a)"                           ,'Note that the input frequency is the ocean wave'
  print "(28x,a)"                           ,'frequency for --swh and the acoustic frequency for'
  print "(28x,a)"                           ,'other types (thus twice the ocean wave frequency).'
  print "(a)", 'Output spectrum conversion:'
  print "(2x,2a)",'-i   , --integrate      ','Integrate spectrum for the provided frequency range.'
  print "(2x,2a)",'-i+  , --integrate+     ','Integrate spectrum and perform additional frequency'
  print "(28x,a)"                           ,'analysis: peak, center, and rms frequency and'
  print "(28x,a)"                           ,'spectral bandwidth.'
  print "(a)", 'Output unit conversion:'
  print "(2x,2a)",'-Pa  , --Pa             ','Set pressure output in Pa. Default unit is log10(Pa).'
  print "(2x,2a)",'-db  , --dB             ','Set pressure output in dB.'
  print "(2x,2a)",'-norm, --normalize      ','Normalize output.'
  print "(2x,2a)",'-var , --variance       ','Square output.'
  print "(a)", 'Output grid conversion:'
  print "(2x,2a)",'-Gr, --grid-regular     ','Project data from a reduced_ll to a regular_ll grid'
  print "(28x,a)"                           ,'with increment equal to the reduced_ll latitude'
  print "(28x,a)"                           ,'increment.'
  print "(2x,2a)",'-Gi, --grid-icosahedron ','Project data to from a reduced_ll to an icosahedron'
  print "(28x,a)"                           ,'grid with edge size equivalent to'
  print "(28x,a)"                           ,'earthRadius * dlat * pi/180.'
  print "(a)", 'Debugging:'
  print "(2x,2a)",'--debug                 ','Verbose everything.'                  
!                                             ***************************************************80
  print "(2x,a)", ''
  stop
!
!*****************************************************************************80
End subroutine oswi_stack_show_help
!*****************************************************************************80
