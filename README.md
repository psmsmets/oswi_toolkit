# OSWI Toolkit

OSWI is a toolbox for Ocean Surface Wave Interaction calculations.

Microbaroms source radiation model using a two-fluid model, over air and seawater, and an ocean-atmosphere model providing the sea state.
Sea state is provided as [2D wave spectra](https://apps.ecmwf.int/codes/grib/param-db/?id=140251).


## Installing pre-requisites using conda

Create and activate the oswi environment:

```
conda create -y -n oswi -c conda-forge make compilers netcdf4 netcdf-fortran eccodes
conda activate oswi
```

### macOS and 10.9 SDK

#### 10.9 SDK
Conda provides the clang compilers for macOS. But the macOS SDK is still required. The SDK license prevents it from being bundled in the conda package. The SDK has to be installed manually. For compatibility issue, conda packages are built with the 10.9 SDK.

The 10.9 SDK can be downloaded from:

https://github.com/devernay/xcodelegacy

https://github.com/phracker/MacOSX-SDKs


Download [MacOSX10.9.sdk.tar.xz](https://github.com/phracker/MacOSX-SDKs/releases) and untar it under `/opt/MacOSX10.9.sdk`.


#### Set environment variables

Before to be able to compile, three variables have to be set on macOS: `MACOSX_DEPLOYMENT_TARGET`, `CONDA_BUILD_SYSROOT`, and `SDKROOT`.


1. Locate the directory for the conda environment in your terminal window by running in the terminal echo `$CONDA_PREFIX`.
2. Enter that directory and create these subdirectories and files:

```
cd $CONDA_PREFIX
mkdir -p ./etc/conda/activate.d
mkdir -p ./etc/conda/deactivate.d
touch ./etc/conda/activate.d/env_vars.sh
touch ./etc/conda/deactivate.d/env_vars.sh
```

3. Edit `./etc/conda/activate.d/env_vars.sh` as follows:

```
#!/bin/sh

export CONDA_BUILD_SYSROOT='/opt/MacOSX10.9.sdk'
export SDKROOT='/opt/MacOSX10.9.sdk'
export MACOSX_DEPLOYMENT_TARGET='10.9'
```

4. Edit `./etc/conda/deactivate.d/env_vars.sh` as follows:

```
#!/bin/sh

unset CONDA_BUILD_SYSROOT
unset SDKROOT
unset MACOSX_DEPLOYMENT_TARGET
```

5. Load environment variables:
```
conda activate oswi
```

## Install source code

### Configure

The fortran compiler via conda is limited to GNU gfortran.
Make an alias to avoid compiler check issues in `configure` (only `gfortran` and not the full name `x86_64-apple-darwin13.4.0-gfortran` is currently allowed).

```
alias gfortran=`$CONDA_PREFIX/bin/nc-config --fc`

FCFLAGS="-O3 -m64 -funroll-all-loops -fpic -I${CONDA_PREFIX}/include"
FCLIBS="-L${CONDA_PREFIX}/lib -lnetcdf -lnetcdff -leccodes_f90 -leccodes"

FC=gfortran FCFLAGS=$FCFLAGS FCLIBS=$FCLIBS ./configure --prefix=$CONDA_PREFIX
```

### Compile and install
 
```
make
make install
```

Make check will give errors due to implementation errors. These can be ignored.


Don't forget to add the variable `OSWI_DEM_FILES` to the oswi environment:
```
echo "export OSWI_DEM_FILES='${CONDA_PREFIX}/share/oswi_toolkit'" >> ${CONDA_PREFIX}/etc/conda/activate.d/env_vars.sh
echo "unset OSWI_DEM_FILES" >> ${CONDA_PREFIX}/etc/conda/deactivate.d/env_vars.sh
```

## Usage

Execute `oswi --help` to display this help text:

```
oswi 0.2.0 - Ocean Surface Wave Interaction core program

Usage: oswi grib_or_nc_files [OPTIONS]

Options and arguments (except files and paths) are not case sensitive.
Mandatory arguments to long options are mandatory for short options too.
General:
  -?, --help              Display this help text.
  -q, --quick             Quick and dirty, avoid all checks.
  -v, --verbose           Verbose information and warnings
  -n, --filename ..       Set filename. Default is "%V__%C_%T_%R__%G__%Y%M%D_%H".
                            Filename variables are:
                             %V = output variable type
                             %A = output variable standard name
                             %G = output grid type
                             %C = 2dfd data class
                             %T = 2dfd data type
                             %Y = year
                             %M = month
                             %D = day of month
                             %O = day of year
                             %H = hour
                             %S = forecast step
                             %N = ensemble number
                             %F = selected frequency
                             %0 = first frequency
                             %1 = last frequency
  -o, --overwrite         Overwrite existing output files, by default not
                            allowed.
  -t, --threads [..]      Max number of openmp processing threads. Default = 1.
Input:
  -f, --frequency .. ..   Specify the frequency range over which the source
                            strength spectrum is calculated. Both upper and lower
                            frequency should be provided. Use -1 to extend the
                            range automatically to the lower / upper limit.
                            Note that the input frequency is the ocean wave
                            frequency for --swh and the acoustic frequency for
                            other types (thus twice the ocean wave frequency).
Environment:
  -id, --infinite-depth   Set infinite ocean. Not for microseisms!
  -fd, --finite-depth     Set finite ocean depth (default). Bathymetry data is
                            obtained from a DEM file specified by either the env
                            variable $OSWI_DEM_FILES or option --dem-file.
  --dem-file ..           Specify path to a DEM file for bathymetry data,
                            ignoring environmental variable $OSWI_DEM_FILES.
  --dem-method ..         Specify downsample method for the given bathymetry
                            data to match the grid of the 2d wave spectra:
                            bilinear interp (default) | mean | min | max.
  --rho-air ..            Set air density (kg m-3).
  --rho-sea ..            Set sea water density (kg m-3).
  --rho-bedrock ..        Set bedrock density (kg m-3).
  --c-air ..              Set speed of sound in air (m s-1).
  --c-sea ..              Set speed of sound in sea water (m s-1).
  --c-bedrock ..          Set bedrock shear velocity (m s-1).
Output type:
  -a, --air               Air surface pressure variation.
  -s, --sea               Sea surface pressure variation.
  -b, --bedrock           Bedrock surface pressure variation.
  -d, --deformation       Sea floor deformation.
  -h, --hass              Hasselmann integral.
  -w, --swh               Significant wave height.
Output spectrum conversion:
  -i   , --integrate      Integrate spectrum for the provided frequency range.
  -i+  , --integrate+     Integrate spectrum and perform additional frequency
                            analysis: peak, center, and rms frequency and
                            spectral bandwidth.
Output unit conversion:
  -Pa  , --Pa             Set pressure output in Pa. Default unit is log10(Pa).
  -db  , --dB             Set pressure output in dB.
  -norm, --normalize      Normalize output.
  -var , --variance       Square output.
Output grid conversion:
  -Gr, --grid-regular     Project data from a reduced_ll to a regular_ll grid
                            with increment equal to the reduced_ll latitude
                            increment.
  -Gi, --grid-icosahedron Project data to from a reduced_ll to an icosahedron
                            grid with edge size equivalent to
                            earthRadius * dlat * pi/180.
Debugging:
  --export-bathymetry     Export used bathymetry after resampling.
  --export-modulation     Export modulation coefficients for choosen output.
  --debug                 Verbose everything.
```

Other oswi toolkit programs are:
- `oswi2ascii` convert oswi netCDF to plain ascii
- `oswi2bin` convert oswi netCDF to binary
- `oswi2kmz` convert a sequence of regularly gridded oswi netCDF files to a kmz movie
- `oswi2seism` convert oswi bedrock netCDF data to microseism forcing and store as binary files
- `oswi_pssp` generate oswi pseudo spectra for a given location and/or area
- `oswi_stack` stack oswi netCDF files

## Reference

> Smets, P. S. M., 2018. Infrasound and the Dynamical Stratosphere: A new application for operational weather and climate prediction, Dissertation, Delft University of Technology. Doi: [10.4233/uuid:517f8597-9c24-4d01-83ed-0f430353e905](https://doi.org/10.4233/uuid:517f8597-9c24-4d01-83ed-0f430353e905)

For additional background information see Section 2.3 in [Smets (2018)](https://doi.org/10.4233/uuid:517f8597-9c24-4d01-83ed-0f430353e905).
