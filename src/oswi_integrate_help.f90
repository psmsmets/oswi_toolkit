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
  print "(a)", ''
  print "(3a)", 'oswi ', trim(liboswi_version()), ' - Ocean Surface Wave Interaction core program'
  print "(a)", ''
  print "(a)", 'Usage: oswi grib_or_nc_files [OPTIONS]'
  print "(a)", ''    
  print "(a)", 'Options and arguments (except files and paths) are not case sensitive.'    
  print "(a)", 'Mandatory arguments to long options are mandatory for short options too.'
  print "(a)", 'General:'
  print "(2x,2a)",'-?, --help              ','Display this help text.'
  print "(2x,2a)",'-q, --quick             ','Quick and dirty, avoid all checks.'
  print "(2x,2a)",'-v, --verbose           ','Verbose information and warnings'
  print "(2x,2a)",'-n, --filename ..       ','Set filename. Default is "%V__%C_%T_%R__%G__%Y%M%D_%H".'
  print "(28x,a)"                           ,'Filename variables are: '
  print "(28x,a)"                           ,' %V = output variable type'
  print "(28x,a)"                           ,' %A = output variable standard name'
  print "(28x,a)"                           ,' %G = output grid type'
  print "(28x,a)"                           ,' %C = 2dfd data class'
  print "(28x,a)"                           ,' %T = 2dfd data type'
  print "(28x,a)"                           ,' %Y = year'
  print "(28x,a)"                           ,' %M = month'
  print "(28x,a)"                           ,' %D = day of month'
  print "(28x,a)"                           ,' %O = day of year'
  print "(28x,a)"                           ,' %H = hour'
  print "(28x,a)"                           ,' %S = forecast step'
  print "(28x,a)"                           ,' %N = ensemble number'
  print "(28x,a)"                           ,' %F = selected frequency'
  print "(28x,a)"                           ,' %0 = first frequency'
  print "(28x,a)"                           ,' %1 = last frequency'
!                                             ***************************************************80
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
  print "(a)", 'Environment:'
  print "(2x,2a)",'-id, --infinite-depth   ','Set infinite ocean. Not for microseisms!'
  print "(2x,2a)",'-fd, --finite-depth     ','Set finite ocean depth (default). Bathymetry data is'
  print "(28x,a)"                           ,'obtained from a DEM file specified by either the env'
  print "(28x,a)"                           ,'variable $OSWI_DEM_FILES or option --dem-file.'
  print "(2x,2a)",'--dem-file ..           ','Specify path to a DEM file for bathymetry data,'
  print "(28x,a)"                           ,'ignoring environmental variable $OSWI_DEM_FILES.'
  print "(2x,2a)",'--dem-method ..         ','Specify downsample method for the given bathymetry'
  print "(28x,a)"                           ,'data to match the grid of the 2d wave spectra: '
  print "(28x,a)"                           ,'bilinear interp (default) | mean | min | max.'
  print "(2x,2a)",'--rho-air ..            ','Set air density (kg m-3).'
  print "(2x,2a)",'--rho-sea ..            ','Set sea water density (kg m-3).'
  print "(2x,2a)",'--rho-bedrock ..        ','Set bedrock density (kg m-3).'
  print "(2x,2a)",'--c-air ..              ','Set speed of sound in air (m s-1).'
  print "(2x,2a)",'--c-sea ..              ','Set speed of sound in sea water (m s-1).'
  print "(2x,2a)",'--c-bedrock ..          ','Set bedrock shear velocity (m s-1).'
  print "(a)", 'Output type:'
  print "(2x,2a)",'-a, --air               ','Air surface pressure variation.'
  print "(2x,2a)",'-s, --sea               ','Sea surface pressure variation.'
  print "(2x,2a)",'-b, --bedrock           ','Bedrock surface pressure variation.'
  print "(2x,2a)",'-d, --deformation       ','Sea floor deformation.'
  print "(2x,2a)",'-h, --hass              ','Hasselmann integral.'
  print "(2x,2a)",'-w, --swh               ','Significant wave height.'
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
  print "(2x,2a)",'--export-bathymetry     ','Export used bathymetry after resampling.'
  print "(2x,2a)",'--export-modulation     ','Export modulation coefficients for choosen output.'
  print "(2x,2a)",'--debug                 ','Verbose everything.'                  
!                                             ***************************************************80
  print "(2x,a)", ''
  stop
!
!*****************************************************************************80
End subroutine oswi_show_help
!*****************************************************************************80
