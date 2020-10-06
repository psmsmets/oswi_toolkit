# configure.ac

AC_PREREQ([2.68])
AC_INIT([oswi_toolkit],
        [0.2.0],
        [mail@pietersmets.be],
        [oswi_toolkit],
        [http://software.pietersmets.be])
AC_CONFIG_SRCDIR([src])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([m4])

# Source file containing package/library versioning information.
 . ${srcdir}/version.sh

OSWI_MAIN_VERSION="${OSWI_MAJOR_VERSION}.${OSWI_MINOR_VERSION}.${OSWI_REVISION_VERSION}"
PACKAGE_VERSION="${OSWI_MAIN_VERSION}"
OSWI_VERSION_STR="${OSWI_MAIN_VERSION}"
OSWI_PATCH_VERSION="${OSWI_REVISION_VERSION}"

AC_SUBST(PACKAGE_NAME)
AC_SUBST(OSWI_MAIN_VERSION)
AC_SUBST(OSWI_VERSION_STR)
AC_SUBST(OSWI_PATCH_VERSION)
AC_SUBST(OSWI_MAJOR_VERSION)
AC_SUBST(OSWI_MINOR_VERSION)

echo "**********************************************"
echo "configuring ${PACKAGE_NAME} v${PACKAGE_VERSION}"
echo "**********************************************"

dnl Sanitize $prefix. Autoconf does this by itself, but so late in the
dnl generated configure script that the expansion does not occur until
dnl after our eval magic below.
AS_IF([test "$prefix" = "NONE"],[prefix=$ac_default_prefix])

SAVE_USER_FCFLAGS=$FCFLAGS
SAVE_USER_FCLIBS=$FCLIBS

################################################################################
# Automake things
################################################################################
# Ensure that make can run correctly
AM_SANITY_CHECK

AM_INIT_AUTOMAKE

AC_DEFINE_UNQUOTED(OSWI_MAIN_VERSION,$OSWI_MAIN_VERSION,OSWI version)
AC_DEFINE_UNQUOTED(OSWI_MAJOR_VERSION,$OSWI_MAJOR_VERSION,OSWI Major release)
AC_DEFINE_UNQUOTED(OSWI_MINOR_VERSION,$OSWI_MINOR_VERSION,OSWI Minor release)
AC_DEFINE_UNQUOTED(OSWI_REVISION_VERSION,$OSWI_REVISION_VERSION,OSWI Revision release)

# Build using AM_SILENT_RULES
AM_SILENT_RULES([yes])


################################################################################
# Libtool
################################################################################

LT_INIT([shared])
LT_LANG(Fortran)
AC_SUBST([LIBTOOL_DEPS])


################################################################################
# Check for programs
################################################################################

AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_LIBTOOL

################################################################################
# Fortran Language
################################################################################

#FCFLAGS="$FCFLAGS -I${prefix}/include -L${prefix}/lib"
#FCFLIBS="$SAVE_USER_FCLIBS"

AC_PROG_FC(pgf90 pgf95 gfortran ifort) # not yet tested: f90 (oracle) xlf90 (ibm)
AC_PROG_FC_C_O

## Set Fortran90 as language!
AC_LANG(Fortran)

################################################################################
# netCDF support
################################################################################

AC_ARG_WITH([nc-config],
  [AS_HELP_STRING([--with-nc-config=PATH],[compile with netCDF4 using nc-config. Optionally specify path to nc-config.])],
				ac_with_nc_config=$withval,ac_with_nc_config='no')
AC_ARG_WITH([netcdf-dir],
  [AS_HELP_STRING([--with-netcdf-dir=DIR],[compile with netCDF4 in DIR])],
				ac_with_netcdf_dir=$withval,ac_with_netcdf_dir='no')
AC_ARG_WITH([netcdf-libdir],
  [AS_HELP_STRING([--with-netcdf-libdir=DIR],[compile with netCDF4 lib in DIR])],
				ac_with_netcdf_libdir=$withval,ac_with_netcdf_libdir='no')
AC_ARG_WITH([netcdf-includedir],
  [AS_HELP_STRING([--with-netcdf-includedir=DIR],[compile with netCDF4 include in DIR])],
				ac_with_netcdf_includedir=$withval,ac_with_netcdf_includedir='no')
case x"${ac_with_nc_config}" in
  xno)  HAVE_NETCDF_CONFIG=no ;;
  xyes)
    AC_CHECK_PROG(HAVE_NETCDF_CONFIG,nc-config,yes,no)
    if test "x$HAVE_NETCDF_CONFIG" = "xno" -a "x$ac_with_nc_config" = "xyes"; then
      AC_MSG_ERROR([*** nc_config for netCDF linking requested but nc_config not found!])
    fi
    nc_config="nc-config"
    ;;
  x*)
    nc_config_basename=$(basename ${ac_with_nc_config})
    nc_config_dirname=$(dirname ${ac_with_nc_config})
    AC_CHECK_PROG(HAVE_NETCDF_CONFIG,${nc_config_basename},yes,no,${nc_config_dirname})
    if test "x$HAVE_NETCDF_CONFIG" = "xno"; then
      AC_MSG_ERROR([*** nc_config for netCDF linking requested but specified nc_config not found!])
    fi
    nc_config=${ac_with_nc_config}
    ;;
esac

NETCDF_FCFLAGS="-lnetcdff -lnetcdf"
NETCDF_FCLIBS=""
if test x"${HAVE_NETCDF_CONFIG}" != x"no"
then
    AC_MSG_CHECKING(for netCDF version)
    NETCDF_VERSION=$(${nc_config} --version | xargs -n1 | tail -1)
    AC_MSG_RESULT($NETCDF_VERSION)

    AC_MSG_CHECKING(for netCDF Fortran compiler)
    NETCDF_FC=$(${nc_config} --fc)
    AC_MSG_RESULT($NETCDF_FC)

    AC_MSG_CHECKING(for netCDF Fortran flags)
    NETCDF_FFLAGS=$(${nc_config} --fflags | xargs -n1 | sort -u | xargs)
    AC_MSG_RESULT($NETCDF_FFLAGS)

    AC_MSG_CHECKING(for netCDF Fortran libs)
    NETCDF_FLIBS=$(${nc_config} --flibs | xargs -n1 | sort -u | xargs)
    AC_MSG_RESULT($NETCDF_FLIBS)

    NETCDF_FCFLAGS="$NETCDF_FFLAGS $NETCDF_FLIBS"
else
  NETCDF_DIR="$prefix"
  if test x"${ac_with_netcdf_dir}" != x"no"
  then
      AC_MSG_CHECKING(for with-netcdf-dir)
      NETCDF_DIR="${ac_with_netcdf_dir%%/}"
      AC_MSG_RESULT($NETCDF_DIR)

      NETCDF_INCLUDEDIR="${NETCDF_DIR}/include"
      NETCDF_LIBDIR="${NETCDF_DIR}/lib"
      NETCDF_FCFLAGS="-L${NETCDF_LIBDIR} -I${NETCDF_INCLUDEDIR} ${NETCDF_FCFLAGS}"
  else
    if test x"${ac_with_netcdf_includedir}" != x"no"
    then
      AC_MSG_CHECKING(for with-netcdf-includedir)
      NETCDF_INCLUDEDIR="${ac_with_netcdf_includedir%%/}"
      NETCDF_DIR="${NETCDF_INCLUDEDIR}/.."
      NETCDF_FCFLAGS="-I${NETCDF_INCLUDEDIR} ${NETCDF_FCFLAGS}"
      AC_MSG_RESULT($NETCDF_INCLUDEDIR)
    fi
    if test x"${ac_with_netcdf_libdir}" != x"no"
    then
      AC_MSG_CHECKING(for with-netcdf-libdir)
      NETCDF_LIBDIR="${ac_with_netcdf_libdir%%/}"
      NETCDF_DIR="${NETCDF_LIBDIR}/.."
      NETCDF_FCFLAGS="-L${NETCDF_LIBDIR} ${NETCDF_FCFLAGS}"
      AC_MSG_RESULT($NETCDF_LIBDIR)
    fi
  fi
  AC_CHECK_PROG(HAVE_NC_CONFIG,nc-config,yes,no,"${NETCDF_DIR}/bin")
  if test x"${HAVE_NC_CONFIG}" == x"yes";
  then
    AC_MSG_CHECKING(for netCDF version)
    NETCDF_VERSION=$(${NETCDF_DIR}/bin/nc-config --version | xargs -n1 | tail -1)
    AC_MSG_RESULT($NETCDF_VERSION)
  fi
fi
SAVE_FCFLAGS=$FCFLAGS
SAVE_FCLIBS=$FCLIBS
AC_LANG_PUSH(Fortran)
#FCLIBS="$FCLIBS"
FCFLAGS="$FCFLAGS $NETCDF_FCFLAGS"
AC_MSG_CHECKING(if netcdf compiles for a simple Fortran 90 program)
AC_LINK_IFELSE([AC_LANG_SOURCE([[
      program main
        use netcdf
        print *, nf90_noerr
      end program main
]])], [AC_MSG_RESULT(yes)], [
  AC_MSG_RESULT(no)
  AC_MSG_ERROR([
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CONFIGURATION ERROR: netCDF test not passed.                                                               
  Please check if the path to the netCDF library is correct.
   1) set the include and lib directory
   2) use nc-config --with-nc-config[=PATH]
   3) by specifying --with-netcdf-dir=DIR
   4) by specifying --with-netcdf-libdir=DIR and --with-netcdf-includedir=DIR
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
])])
FCFLAGS=$SAVE_FCFLAGS
FCLIBS=$SAVE_FCLIBS
AC_LANG_POP(Fortran)


################################################################################
# eccodes support
################################################################################

AC_ARG_WITH([eccodes-dir],
  [AS_HELP_STRING([--with-eccodes-dir=DIR],[compile with eccodes in DIR])],
				ac_with_eccodes_dir=$withval,ac_with_eccodes_dir='no')
AC_ARG_WITH([eccodes-libdir],
  [AS_HELP_STRING([--with-eccodes-libdir=DIR],[compile with eccodes lib in DIR])],
				ac_with_eccodes_libdir=$withval,ac_with_eccodes_libdir='no')
AC_ARG_WITH([eccodes-includedir],
  [AS_HELP_STRING([--with-eccodes-includedir=DIR],[compile with eccodes include in DIR])],
				ac_with_eccodes_includedir=$withval,ac_with_eccodes_includedir='no')
ECCODES_DIR="$prefix"
ECCODES_FCFLAGS="-leccodes_f90 -leccodes"
ECCODES_FCLIBS=""
if test x"${ac_with_eccodes_dir}" != x"no"
then
    AC_MSG_CHECKING(for with-eccodes-dir)
    ECCODES_DIR="${ac_with_eccodes_dir%%/}"
    AC_MSG_RESULT($ECCODES_DIR)
    ECCODES_FCFLAGS="-I${ECCODES_DIR}/include -L${ECCODES_DIR}/lib -Wl,-rpath,${ECCODES_DIR}/lib ${ECCODES_FCFLAGS}"
else
  if test x"${ac_with_eccodes_libdir}" != x"no"
  then
    AC_MSG_CHECKING(for with-eccodes-libdir)
    ECCODES_LIBDIR="${ac_with_eccodes_libdir%%/}"
    AC_MSG_RESULT($ECCODES_LIBDIR)
    ECCODES_DIR="${ECCODES_LIBDIR}../bin"
    ECCODES_FCFLAGS="-L${ECCODES_LIBDIR} -Wl,-rpath,${ECCODES_LIBDIR}/lib ${ECCODES_FCFLAGS}"
  fi
  if test x"${ac_with_eccodes_includedir}" != x"no"
  then
    AC_MSG_CHECKING(for with-eccodes-includedir)
    ECCODES_INCLUDEDIR="${ac_with_eccodes_includedir%%/}"
    AC_MSG_RESULT($ECCODES_INCLUDEDIR)
    ECCODES_DIR="${ECCODES_INCLUDEDIR}../bin"
    ECCODES_FCFLAGS="-I${ECCODES_INCLUDEDIR} ${ECCODES_FCFLAGS}"
  fi
fi
AC_CHECK_PROG(HAVE_ECCODES_INFO,grib_ls,yes,no,"${ECCODES_DIR}/bin")
if test x"${HAVE_ECCODES_INFO}" == x"yes";
then
  AC_MSG_CHECKING(for eccodes version)
  ECCODES_VERSION=$(${ECCODES_DIR}/bin/grib_ls -V | grep -v ^$ | xargs -n1 | tail -1)
  AC_MSG_RESULT($ECCODES_VERSION)
fi
SAVE_FCFLAGS=$FCFLAGS
SAVE_FCLIBS=$FCLIBS
FCFLAGS="$FCFLAGS $ECCODES_FCFLAGS"
FCLIBS="$FCLIBS"
AC_LANG_PUSH(Fortran)
AC_MSG_CHECKING(if eccodes compiles for a simple Fortran 90 program)
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
      program main
        use eccodes
        call codes_release(1) 
        call grib_release(1) 
      end program main
]])], [AC_MSG_RESULT(yes)], [
  AC_MSG_RESULT(no)
  AC_MSG_ERROR([
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CONFIGURATION ERROR: eccodes test not passed.                                                               
  Please check that the path to the eccodes library is correct.
   1) set the include and lib directory
   2) by specifying --with-eccodes-dir=DIR
   3) by specifying --with-eccodes-libdir=DIR and --with-eccodes-includedir=DIR
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
])])
FCFLAGS=$SAVE_FCFLAGS
FCLIBS=$SAVE_FCLIBS
AC_LANG_POP(Fortran)


################################################################################
# Merge FCLIBS and FCFLAGS
################################################################################

OSWI_FCFLAGS=$(echo "${NETCDF_FCFLAGS} ${ECCODES_FCFLAGS}" | xargs -n1 | sort -u | xargs)
OSWI_FCLIBS=$(echo "${NETCDF_FCLIBS} ${ECCODES_FCLIBS}" | xargs -n1 | sort -u | xargs)


################################################################################
# Compiling settings
################################################################################

AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],[Enable debug mode])],
[case "${enableval}" in
  yes) debug=true ;;
  no)  debug=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-debug]) ;;
esac],[debug=false])
AM_CONDITIONAL([HAVE_DEBUG], [test x$debug = xtrue])

AC_ARG_ENABLE([stack],
  [AS_HELP_STRING([--enable-stack],[Enable increased stack size])],
[case "${enableval}" in
  yes) stack=true ;;
  no)  stack=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-stack]) ;;
esac],[stack=false])
AM_CONDITIONAL([HAVE_STACK], [test x$stack = xtrue])

AC_ARG_ENABLE([ldflags],
  [AS_HELP_STRING([--enable-ldflags],[Enable loading library ldflags])],
[case "${enableval}" in
  yes) ldflags=true; AC_FC_LIBRARY_LDFLAGS ;;
  no)  ldfalgs=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-ldflags]) ;;
esac],[ldflags=false])
AM_CONDITIONAL([HAVE_LDFLAGS], [test x$ldflags = xtrue])


# get fc name only (no path)
fc_test=$(basename ${FC});


# openmp required at the moment ...
case "${fc_test}" in
  gfortran) OSWI_FCFLAGS="${OSWI_FCFLAGS} -fopenmp";;
  ifort) OSWI_FCFLAGS="${OSWI_FCFLAGS} -fast -openmp";;
  pgf9*) OSWI_FCFLAGS="${OSWI_FCFLAGS} -mp";;
  *) AC_MSG_ERROR([Unsupported compiler!]) ;;
esac

FCFLAGS="$OSWI_FCFLAGS"
FCLIBS="$OSWI_FCLIBS"

if test "x${SAVE_USER_FCFLAGS}" != "x";
then
  FCFLAGS="$FCFLAGS $SAVE_USER_FCFLAGS"
else
  # default or debug mode
  if test "x$debug" == "xtrue";
  then
    case "${fc_test}" in
      gfortran) FCFLAGS="$FCFLAGS -Og -g -fbounds-check -Wall -Waliasing -Wsurprising -Wextra -pedantic -fimplicit-none -ffpe-trap=zero,overflow,underflow,invalid -finit-real=nan -fcheck=all -fbacktrace";;
      ifort) FCFLAGS="$FCFLAGS -O0 -check bounds -g -traceback -warn all -debug extended -save-temps -fp-stack-check -gen-interfaces -fstack-protector -check arg_temp_created";;
      pgf9*) FCFLAGS="$FCFLAGS -O0 -g -m64 -traceback -Mbounds";;
      *) AC_MSG_ERROR([Unsupported compiler!]) ;;
    esac
  else
    case "${fc_test}" in
      gfortran) FCFLAGS="$FCFLAGS -m64 -O3 -funroll-all-loops -fpic";;
      ifort) FCFLAGS="$FCFLAGS -m64 -fast";;
      pgf9*) FCFLAGS="$FCFLAGS -m64 -O3 -fastsse";;
      *) AC_MSG_ERROR([Unsupported compiler!]) ;;
    esac
  fi
  # increased stack size
  if test x$stack = xtrue;
  then
    case "${fc_test}" in
      gfortran) FCFLAGS="$FCFLAGS -fmax-stack-var-size=1073741824";;
      ifort) FCFLAGS="$FCFLAGS -Wl,-stack_size,0x40000000,-stack_addr,0xc0000000";;
      pgf9*) FCFLAGS="$FCFLAGS -Wl,-stack_size,0x10000000,-stack_addr,0xc0000000";;
      *) AC_MSG_ERROR([Unsupported compiler!]) ;;
    esac
  fi
fi

if test "x${SAVE_USER_FCLIBS}" != "x";
then
  FCLIBS="$FCLIBS $SAVE_USER_FCLIBS"
fi
LIBS=$FCLIBS


################################################################################
# Linux distribution
################################################################################

#AX_LINUX_DISTRIBUTION


################################################################################
# For package info (quo-info)
################################################################################

QUO_WHICH_FC=`which \`echo "$FC" | tr -s ' ' | cut -f1 -d' '\``
if test "x$QUO_WHICH_FC" = x; then
    QUO_WHICH_FC="none"
fi

AC_SUBST([QUO_WHICH_FC])
AC_SUBST([FCFLAGS])
AC_SUBST([FCLIBS])

AC_CONFIG_FILES([Makefile
                 data/Makefile
                 script/Makefile
                 src/Makefile
                 tests/Makefile])
AC_OUTPUT

AC_MSG_NOTICE([

  Configuration completed.

+------------------------------------------------------------------------------+
|                 Ocean Surface Wave Interaction - Toolkit                     |
+------------------------------------------------------------------------------+
 
  version        : ${OSWI_MAIN_VERSION}
  target         : ${prefix}
  with netCDF    : ${NETCDF_VERSION}
  with eccodes   : ${ECCODES_VERSION}
  FC             : ${FC}
  FCFLAGS        : ${FCFLAGS}
  FCLIBS         : ${FCLIBS}

+------------------------------------------------------------------------------+

    You can now say 'make' to compile the oswi_toolkit package, 'make check' to
	  test it  and 'make install' to install it afterwards.

	  ])
