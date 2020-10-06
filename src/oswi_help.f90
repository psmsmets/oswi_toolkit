!*****************************************************************************80
!
!                                O S W I _ H E L P
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
Subroutine oswi_show_help ( )
!*****************************************************************************80
!
! Subroutine oswi_show_help
!
! Description
!
!   Show oswi help text
!
!*****************************************************************************80
!
  write (output_unit,"(a)") ''
  print "(3a)", 'oswi ', trim(liboswi_version()), ' - Ocean Surface Wave Interaction core program'
  write (output_unit,"(a)") ''
  write (output_unit,"(a)") 'Usage: oswi grib_or_nc_files [OPTIONS]'
  write (output_unit,"(a)") ''    
  write (output_unit,"(a)") 'Options and arguments (except files and paths) are not case sensitive.'    
  write (output_unit,"(a)") 'Mandatory arguments to long options are mandatory for short options too.'
  write (output_unit,"(a)") 'General:'
  write (output_unit,"(2x,2a)")'-?, --help              ','Display this help text.'
  write (output_unit,"(2x,2a)")'-q, --quick             ','Quick and dirty, avoid all checks.'
  write (output_unit,"(2x,2a)")'-v, --verbose           ','Verbose information and warnings'
  write (output_unit,"(2x,2a)")'-n, --filename ..       ','Set filename. Default is "%V__%C_%T_%R__%G__%Y%M%D_%H".'
  write (output_unit,"(28x,a)")                           'Filename variables are: '
  write (output_unit,"(28x,a)")                           ' %V = output variable type'
  write (output_unit,"(28x,a)")                           ' %A = output variable standard name'
  write (output_unit,"(28x,a)")                           ' %G = output grid type'
  write (output_unit,"(28x,a)")                           ' %C = 2dfd data class'
  write (output_unit,"(28x,a)")                           ' %T = 2dfd data type'
  write (output_unit,"(28x,a)")                           ' %Y = year'
  write (output_unit,"(28x,a)")                           ' %M = month'
  write (output_unit,"(28x,a)")                           ' %D = day of month'
  write (output_unit,"(28x,a)")                           ' %O = day of year'
  write (output_unit,"(28x,a)")                           ' %H = hour'
  write (output_unit,"(28x,a)")                           ' %S = forecast step'
  write (output_unit,"(28x,a)")                           ' %N = ensemble number'
  write (output_unit,"(28x,a)")                           ' %F = selected frequency'
  write (output_unit,"(28x,a)")                           ' %0 = first frequency'
  write (output_unit,"(28x,a)")                           ' %1 = last frequency'
!                                             ***************************************************80
  write (output_unit,"(2x,2a)")'-o, --overwrite         ','Overwrite existing output files, by default not'
  write (output_unit,"(28x,a)")                           'allowed.'
  write (output_unit,"(2x,2a)")'-t, --threads [..]      ','Max number of openmp processing threads. Default = 1.'
  write (output_unit,"(a)") 'Input:'
  write (output_unit,"(2x,2a)")'-f, --frequency .. ..   ','Specify the frequency range over which the source'
  write (output_unit,"(28x,a)")                           'strength spectrum is calculated. Both upper and lower'
  write (output_unit,"(28x,a)")                           'frequency should be provided. Use -1 to extend the'
  write (output_unit,"(28x,a)")                           'range automatically to the lower / upper limit.'
  write (output_unit,"(28x,a)")                           'Note that the input frequency is the ocean wave'
  write (output_unit,"(28x,a)")                           'frequency for --swh and the acoustic frequency for'
  write (output_unit,"(28x,a)")                           'other types (thus twice the ocean wave frequency).'
  write (output_unit,"(a)") 'Environment:'
  write (output_unit,"(2x,2a)")'-id, --infinite-depth   ','Set infinite ocean. Not for microseisms!'
  write (output_unit,"(2x,2a)")'-fd, --finite-depth     ','Set finite ocean depth (default). Bathymetry data is'
  write (output_unit,"(28x,a)")                           'obtained from a DEM file specified by either the env'
  write (output_unit,"(28x,a)")                           'variable $OSWI_DEM_FILES or option --dem-file.'
  write (output_unit,"(2x,2a)")'--dem-file ..           ','Specify path to a DEM file for bathymetry data,'
  write (output_unit,"(28x,a)")                           'ignoring environmental variable $OSWI_DEM_FILES.'
  write (output_unit,"(2x,2a)")'--dem-method ..         ','Specify downsample method for the given bathymetry'
  write (output_unit,"(28x,a)")                           'data to match the grid of the 2d wave spectra: '
  write (output_unit,"(28x,a)")                           'bilinear interp (default) | mean | min | max.'
  write (output_unit,"(2x,2a)")'--rho-air ..            ','Set air density (kg m-3).'
  write (output_unit,"(2x,2a)")'--rho-sea ..            ','Set sea water density (kg m-3).'
  write (output_unit,"(2x,2a)")'--rho-bedrock ..        ','Set bedrock density (kg m-3).'
  write (output_unit,"(2x,2a)")'--c-air ..              ','Set speed of sound in air (m s-1).'
  write (output_unit,"(2x,2a)")'--c-sea ..              ','Set speed of sound in sea water (m s-1).'
  write (output_unit,"(2x,2a)")'--c-bedrock ..          ','Set bedrock shear velocity (m s-1).'
  write (output_unit,"(a)") 'Output type:'
  write (output_unit,"(2x,2a)")'-a, --air               ','Air surface pressure variation.'
  write (output_unit,"(2x,2a)")'-s, --sea               ','Sea surface pressure variation.'
  write (output_unit,"(2x,2a)")'-b, --bedrock           ','Bedrock surface pressure variation.'
  write (output_unit,"(2x,2a)")'-d, --deformation       ','Sea floor deformation.'
  write (output_unit,"(2x,2a)")'-h, --hass              ','Hasselmann integral.'
  write (output_unit,"(2x,2a)")'-w, --swh               ','Significant wave height.'
  write (output_unit,"(a)") 'Output spectrum conversion:'
  write (output_unit,"(2x,2a)")'-i   , --integrate      ','Integrate spectrum for the provided frequency range.'
  write (output_unit,"(2x,2a)")'-i+  , --integrate+     ','Integrate spectrum and perform additional frequency'
  write (output_unit,"(28x,a)")                           'analysis: peak, center, and rms frequency and'
  write (output_unit,"(28x,a)")                           'spectral bandwidth.'
  write (output_unit,"(a)") 'Output unit conversion:'
  write (output_unit,"(2x,2a)")'-Pa  , --Pa             ','Set pressure output in Pa. Default unit is log10(Pa).'
  write (output_unit,"(2x,2a)")'-db  , --dB             ','Set pressure output in dB.'
  write (output_unit,"(2x,2a)")'-norm, --normalize      ','Normalize output.'
  write (output_unit,"(2x,2a)")'-var , --variance       ','Square output.'
  write (output_unit,"(a)") 'Output grid conversion:'
  write (output_unit,"(2x,2a)")'-Gr, --grid-regular     ','Project data from a reduced_ll to a regular_ll grid'
  write (output_unit,"(28x,a)")                           'with increment equal to the reduced_ll latitude'
  write (output_unit,"(28x,a)")                           'increment.'
  write (output_unit,"(2x,2a)")'-Gi, --grid-icosahedron ','Project data to from a reduced_ll to an icosahedron'
  write (output_unit,"(28x,a)")                           'grid with edge size equivalent to'
  write (output_unit,"(28x,a)")                           'earthRadius * dlat * pi/180.'
  write (output_unit,"(a)") 'Debugging:'
  write (output_unit,"(2x,2a)")'--export-bathymetry     ','Export used bathymetry after resampling.'
  write (output_unit,"(2x,2a)")'--export-modulation     ','Export modulation coefficients for choosen output.'
  write (output_unit,"(2x,2a)")'--debug                 ','Verbose everything.'                  
!                                             ***************************************************80
  write (output_unit,"(2x,a)") ''
  stop
!
!*****************************************************************************80
End subroutine oswi_show_help
!*****************************************************************************80
