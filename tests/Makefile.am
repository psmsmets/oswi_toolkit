TESTS = oswi.sh \
        oswi2ascii.sh \
        oswi2bin.sh \
        oswi_stack.sh

# First download all the necessary data for testing
# Note: if download fails, no tests will be done
#$(TESTS): download_data
#download_data:
#	@$(top_srcdir)/tests/download.sh $(top_srcdir)/tests

noinst_PROGRAMS = 

LDADD = $(top_builddir)/src/libgrib_api.la $(EMOS_LIB)

AM_CPPFLAGS  = -I$(top_builddir)/src

EXTRA_DIST = $(TESTS) include.sh utils.sh
