ARCH_TAG     = _sgi
ARFLAGS      = vqs
FFLAGS       = -c -O -w
FFLAGS_DBG   = -c -g -w
CCFLAGS      = -c -O -ansi -fullwarn -v -prototypes
CCFLAGS_DBG  = -c -g -ansi -fullwarn -v -prototypes

all : libstr.a libstr_dbg.a

str : libstr.a

dbg : libstr_dbg.a


libstr.a : strlib.o strlib$(ARCH_TAG).o strmth.o strplot.o strterm$(ARCH_TAG).o str_nextarg.o
libstr.a : strbyte$(ARCH_TAG).o strtermc$(ARCH_TAG).o
	rm -f libstr.a
	ar $(ARFLAGS) libstr.a strlib.o strlib$(ARCH_TAG).o strmth.o strplot.o \
	strterm$(ARCH_TAG).o str_nextarg.o \
	strbyte$(ARCH_TAG).o strtermc$(ARCH_TAG).o
	ranlib libstr.a

libstr_dbg.a : strlib_dbg.o strlib$(ARCH_TAG)_dbg.o strmth_dbg.o strplot_dbg.o strterm$(ARCH_TAG)_dbg.o str_nextarg_dbg.o
libstr_dbg.a : strbyte$(ARCH_TAG)_dbg.o strtermc$(ARCH_TAG)_dbg.o
	rm -f libstr_dbg.a
	ar $(ARFLAGS) libstr_dbg.a strlib_dbg.o strlib$(ARCH_TAG)_dbg.o strmth_dbg.o strplot_dbg.o \
	strterm$(ARCH_TAG)_dbg.o str_nextarg_dbg.o \
	strbyte$(ARCH_TAG)_dbg.o strtermc$(ARCH_TAG)_dbg.o
	ranlib libstr_dbg.a


strlib.o : strlib.f strlib_inc
	f77 $(FFLAGS) strlib.f

strlib_dbg.o : strlib.f strlib_inc
	f77 $(FFLAGS_DBG) strlib.f -o strlib_dbg.o


strlib$(ARCH_TAG).o : strlib$(ARCH_TAG).f
	f77 $(FFLAGS) strlib$(ARCH_TAG).f

strlib$(ARCH_TAG)_dbg.o : strlib$(ARCH_TAG).f
	f77 $(FFLAGS_DBG) strlib$(ARCH_TAG).f -o strlib$(ARCH_TAG)_dbg.o


strmth.o : strmth.f
	f77 $(FFLAGS) strmth.f

strmth_dbg.o : strmth.f
	f77 $(FFLAGS_DBG) strmth.f -o strmth_dbg.o


strplot.o : strplot.f
	f77 $(FFLAGS) strplot.f

strplot_dbg.o : strplot.f
	f77 $(FFLAGS_DBG) strplot.f -o strplot_dbg.o


strterm$(ARCH_TAG).o : strterm$(ARCH_TAG).f
	f77 $(FFLAGS) strterm$(ARCH_TAG).f

strterm$(ARCH_TAG)_dbg.o : strterm$(ARCH_TAG).f
	f77 $(FFLAGS_DBG) strterm$(ARCH_TAG).f -o strterm$(ARCH_TAG)_dbg.o


str_nextarg.o : str_nextarg.f str_ret_inc
	f77 $(FFLAGS) str_nextarg.f

str_nextarg_dbg.o : str_nextarg.f str_ret_inc
	f77 $(FFLAGS_DBG) str_nextarg.f -o str_nextarg_dbg.o


strbyte$(ARCH_TAG).o : strbyte$(ARCH_TAG).c
	cc $(CCFLAGS) strbyte$(ARCH_TAG).c

strbyte$(ARCH_TAG)_dbg.o : strbyte$(ARCH_TAG).c
	cc $(CCFLAGS_DBG) strbyte$(ARCH_TAG).c -o strbyte$(ARCH_TAG)_dbg.o


strtermc$(ARCH_TAG).o : strtermc$(ARCH_TAG).c
	cc $(CCFLAGS) strtermc$(ARCH_TAG).c

strtermc$(ARCH_TAG)_dbg.o : strtermc$(ARCH_TAG).c
	cc $(CCFLAGS_DBG) strtermc$(ARCH_TAG).c -o strtermc$(ARCH_TAG)_dbg.o



strlib_inc : strlib.inc
	ln -s strlib.inc strlib_inc

str_ret_inc : str_ret.inc
	ln -s str_ret.inc str_ret_inc


strterm_sun.f : strterm_sgi.f
	rm -f strterm_sun.f
	ln -s strterm_sgi.f strterm_sun.f

strtermc_sun.c : strtermc_sgi.c
	rm -f strtermc_sun.c
	ln -s strtermc_sgi.c strtermc_sun.c

strbyte_sun.c : strbyte_sgi.c
	rm -f strbyte_sun.c
	ln -s strbyte_sgi.c strbyte_sun.c
