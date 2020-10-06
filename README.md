# OSWI Toolkit

OSWI is a toolbox for Ocean Surface Wave Interaction calculations.

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
conda install -c conda-forge compilers netcfd4 netcdf-fortran eccodes
```

## Install source code

The fortran compiler is limited to GNU gfortran (via conda).
Make an alias to avoid compiler check issues in the `configure`.

### Configure

```
alias gfortran=`$CONDA_PREFIX/bin/nc-config --fc`

FC=gfortran
FCFLAGS="-O3 -m64 -funroll-all-loops -fpic -I${CONDA_PREFIX}/include"
FCLIBS="-L${CONDA_PREFIX}/lib -lnetcdf -lnetcdff -leccodes_f90 -leccodes"

FC=$FC FCFLAGS=$FCFLAGS FCLIBS=$FCLIBS ./configure --prefix=$CONDA_PREFIX
```

### Compile and install
 
```
make
make install
```

Make check will give errors due to implementation errors. These can be ignored.


Don't forget to add `OSWI_DEM_FILES=${CONDA_PREFIX}/share/oswi_toolkit` to your environment.
Checkout https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html#macos-and-linux how to add a variable to your environment.
