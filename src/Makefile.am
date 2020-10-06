#AM_FCFLAGS=-I$(top_srcdir)/src

# Some Fortran compilers can't build shared libraries, so sometimes we
# want to build a shared C library and a static Fortran library.  If so,
# pass the -static flag to the library linker.
#if FORTRAN_SHARED_CONDITIONAL
#else
#   AM_LDFLAGS+=-static
#endif

bin_PROGRAMS = oswi oswi_stack oswi2bin oswi2ascii oswi2seism oswi_pssp

oswi_SOURCES = io_types.f90 string_utility.f90 byteswap.f90 syms.f90 time.f90 sort.f90 file_utility.f90 \
		math.f90 netcdf_ext.f90 libdem.f90 grib_types.f90 grib_2dfd.f90 sphere_grid.f90 \
		liboswi.f90 oswi.f90

oswi_stack_SOURCES = io_types.f90 string_utility.f90 byteswap.f90 syms.f90 time.f90 sort.f90 \
			file_utility.f90 math.f90 netcdf_ext.f90 libdem.f90 grib_types.f90 sphere_grid.f90 \
			liboswi.f90 oswi_stack.f90

oswi2bin_SOURCES = io_types.f90 string_utility.f90 byteswap.f90 syms.f90 time.f90 sort.f90 \
			file_utility.f90 math.f90 netcdf_ext.f90 libdem.f90 grib_types.f90 sphere_grid.f90 \
			liboswi.f90 oswi2bin.f90

oswi2ascii_SOURCES = io_types.f90 string_utility.f90 byteswap.f90 syms.f90 time.f90 sort.f90 \
			file_utility.f90 math.f90 netcdf_ext.f90 libdem.f90 grib_types.f90 sphere_grid.f90 \
			liboswi.f90 oswi2ascii.f90

oswi2seism_SOURCES = io_types.f90 string_utility.f90 byteswap.f90 syms.f90 time.f90 sort.f90 \
			file_utility.f90 math.f90 netcdf_ext.f90 libdem.f90 grib_types.f90 sphere_grid.f90 \
			liboswi.f90 oswi2seism.f90

oswi_pssp_SOURCES = io_types.f90 string_utility.f90 byteswap.f90 syms.f90 time.f90 sort.f90 \
			file_utility.f90 math.f90 netcdf_ext.f90 libdem.f90 grib_types.f90 sphere_grid.f90 \
			liboswi.f90 oswi_pssp.f90


io_types.o : io_types.f90
string_utility.o : string_utility.f90
syms.o : syms.f90 io_types.o
byteswap.o : byteswap.f90 io_types.o
time.o : time.f90 io_types.o
sort.o : sort.f90 io_types.o
file_utility.o : file_utility.f90 io_types.o
math.o : math.f90 io_types.o syms.o
netcdf_ext.o : netcdf_ext.f90 io_types.o
libdem.o : libdem.f90 io_types.o string_utility.o math.o netcdf_ext.o
grib_types.o : grib_types.f90 io_types.o
grib_2dfd.o : grib_2dfd.f90 io_types.o syms.o time.o grib_types.o
sphere_grid.o : sphere_grid.f90
liboswi.o : liboswi.f90 io_types.o time.o syms.o math.o netcdf_ext.o grib_types.o grib_2dfd.o \
		libdem.o sphere_grid.o

oswi.o : oswi.f90 io_types.o string_utility.o file_utility.o sort.o time.o libdem.o grib_2dfd.o liboswi.o
oswi_LDADD = 
oswi_LDFLAGS = -lnetcdf -lnetcdff -leccodes_f90 -leccodes

oswi_stack.o : oswi_stack.f90 io_types.o string_utility.o file_utility.o sort.o time.o liboswi.o
oswi_stack_LDADD = 
#oswi_stack_LDFLAGS = -lnetcdf -lnetcdff

oswi2bin.o : oswi2bin.f90 io_types.o string_utility.o file_utility.o sort.o time.o math.o liboswi.o
oswi2bin_LDADD = 
#oswi2bin_LDFLAGS = -lnetcdf -lnetcdff

oswi2ascii.o : oswi2ascii.f90 io_types.o string_utility.o file_utility.o sort.o time.o math.o liboswi.o
oswi2ascii_LDADD = 
#oswi2ascii_LDFLAGS = -lnetcdf -lnetcdff

oswi2seism.o : oswi2seism.f90 io_types.o string_utility.o file_utility.o sort.o time.o math.o liboswi.o
oswi2seism_LDADD = 
#oswi2seism_LDFLAGS = -lnetcdf -lnetcdff

oswi_pssp.o : oswi_pssp.f90 io_types.o string_utility.o file_utility.o sort.o time.o math.o liboswi.o
oswi_pssp_LDADD = 
oswi_pssp_LDFLAGS = -lnetcdf -lnetcdff -leccodes_f90 -leccodes


CLEANFILES =	*.mod
EXTRA_DIST =	Rayleigh_source_c_1500_3000.f90 adaguc_standards.f90 dem_set_oswi.f90 grib_set_oswi.f90 \
		liboswi_debug_bathymetry_modulation.f90 liboswi_def_parameters.f90 \
		liboswi_def_process.f90 liboswi_def_readwrite.f90 liboswi_def_types.f90 \
		liboswi_def_variables.f90 liboswi_export_oswi.f90 liboswi_grid_transform.f90 \
                liboswi_filename.f90 liboswi_export_oswi_stack.f90 \
		liboswi_grid_transform_fun.f90 liboswi_in.f90 liboswi_in_grib.f90 liboswi_in_nc.f90 \
		liboswi_modulate.f90 liboswi_ncfile.f90 liboswi_process.f90 liboswi_read.f90 \
		liboswi_set_types.f90 liboswi_types.f90 liboswi_version.f90 liboswi_write.f90 \
		oswi_argc.f90 oswi_defaults.f90 oswi_help.f90 \
		oswi_stack_help.f90 oswi_stack_defaults.f90 oswi_stack_argc.f90 \
		oswi_integrate_help.f90 oswi_integrate_defaults.f90 oswi_integrate_argc.f90 \
		oswi2bin_command_args.f90 oswi2bin_help.f90 \
		oswi2ascii_command_args.f90 oswi2ascii_help.f90 \
		oswi2seism_command_args.f90 oswi2seism_help.f90 \
		oswi_pssp_command_args.f90 oswi_pssp_help.f90
