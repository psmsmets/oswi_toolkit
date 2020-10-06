LIBTOOL_DEPS = @LIBTOOL_DEPS@
libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./configure_status libtool
ACLOCAL_AMFLAGS = -I m4

CONFIGURE_DEPENDENCIES = $(top_srcdir)/version.sh

$(top_srcdir)/configure: $(top_srcdir)/version.sh

AUTOMAKE_OPTIONS = foreign
SUBDIRS = data script src tests
DIST_SUBDIRS = data script src tests

install-exec-local:
	@echo ""
	@echo "    ** oswi_toolkit make install complete **"
	@echo ""
	@echo "    Don't forget to set your enviroment variable!"
	@echo "      OSWI_DEM_FILES=${datarootdir}/oswi_toolkit"
	@echo ""
