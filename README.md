# OSWI Toolkit

OSWI is a toolbox for Ocean Surface Wave Interaction calculations.

Microbaroms source radiation model using a two-fluid model, over air and seawater, and an ocean-atmosphere model providing the sea state.
Sea state is provided as [2D wave spectra](https://apps.ecmwf.int/codes/grib/param-db/?id=140251).


## Install dependencies using conda

Create a new conda environment
```
conda create --name=oswi
```

Activate `oswi` environment
```
conda activate oswi
```

Add dependencies
```
conda install -c conda-forge compilers netcdf4 netcdf-fortran eccodes
```

*Important* for macOS users: make shure you have [macOS 10.9 SDK](https://docs.conda.io/projects/conda-build/en/latest/resources/compiler-tools.html#macos-sdk) available on your system. Some extra info [here](https://github.com/ContinuumIO/anaconda-issues/issues/9096). If added don't forget to set `export CFLAGS="${CFLAGS} -isysroot ${CONDA_BUILD_SYSROOT}"`.

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


Don't forget to add `OSWI_DEM_FILES=${CONDA_PREFIX}/share/oswi_toolkit` to your environment.
Checkout https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html#macos-and-linux how to add a variable to your environment.


## Reference

> Smets, P. S. M., 2018. Infrasound and the Dynamical Stratosphere: A new application for operational weather and climate prediction, Dissertation, Delft University of Technology. Doi: [10.4233/uuid:517f8597-9c24-4d01-83ed-0f430353e905](https://doi.org/10.4233/uuid:517f8597-9c24-4d01-83ed-0f430353e905)

For additional background information see Section 2.3 in [Smets (2018)](https://doi.org/10.4233/uuid:517f8597-9c24-4d01-83ed-0f430353e905).
