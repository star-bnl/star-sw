MSG = ../msg

ARFLAGS      = vqs
FFLAGS       = -c -O -w
FFLAGS_DBG   = -c -g -w
CCFLAGS      = -c -O -w
CCFLAGS_DBG  = -c -g -w

all : libstr.a libstr_dbg.a

str : libstr.a

dbg : libstr_dbg.a


libstr.a : strlib.o strlib_sgi.o strmth.o strplot.o strterm_sgi.o str_nextarg.o
libstr.a : strbyte_sgi.o strtermc_sgi.o
	rm -f libstr.a
	ar $(ARFLAGS) libstr.a strlib.o strlib_sgi.o strmth.o strplot.o \
	strterm_sgi.o str_nextarg.o \
	strbyte_sgi.o strtermc_sgi.o
	ranlib libstr.a

libstr_dbg.a : strlib_dbg.o strlib_sgi_dbg.o strmth_dbg.o strplot_dbg.o strterm_sgi_dbg.o str_nextarg_dbg.o
libstr_dbg.a : strbyte_sgi_dbg.o strtermc_sgi_dbg.o
	rm -f libstr_dbg.a
	ar $(ARFLAGS) libstr_dbg.a strlib_dbg.o strlib_sgi_dbg.o strmth_dbg.o strplot_dbg.o \
	strterm_sgi_dbg.o str_nextarg_dbg.o \
	strbyte_sgi_dbg.o strtermc_sgi_dbg.o
	ranlib libstr_dbg.a


strlib.o : strlib.f strlib_inc
	f77 $(FFLAGS) strlib.f

strlib_dbg.o : strlib.f strlib_inc
	f77 $(FFLAGS_DBG) strlib.f -o strlib_dbg.o


strlib_sgi.o : strlib_sgi.f
	f77 $(FFLAGS) strlib_sgi.f

strlib_sgi_dbg.o : strlib_sgi.f
	f77 $(FFLAGS_DBG) strlib_sgi.f -o strlib_sgi_dbg.o


strmth.o : strmth.f
	f77 $(FFLAGS) strmth.f

strmth_dbg.o : strmth.f
	f77 $(FFLAGS_DBG) strmth.f -o strmth_dbg.o


strplot.o : strplot.f
	f77 $(FFLAGS) strplot.f

strplot_dbg.o : strplot.f
	f77 $(FFLAGS_DBG) strplot.f -o strplot_dbg.o


strterm_sgi.o : strterm_sgi.f
	f77 $(FFLAGS) strterm_sgi.f

strterm_sgi_dbg.o : strterm_sgi.f
	f77 $(FFLAGS_DBG) strterm_sgi.f -o strterm_sgi_dbg.o


str_nextarg.o : str_nextarg.f str_ret_inc
	f77 $(FFLAGS) str_nextarg.f

str_nextarg_dbg.o : str_nextarg.f str_ret_inc
	f77 $(FFLAGS_DBG) str_nextarg.f -o str_nextarg_dbg.o


strbyte_sgi.o : strbyte_sgi.c
	cc $(CCFLAGS) strbyte_sgi.c

strbyte_sgi_dbg.o : strbyte_sgi.c
	cc $(CCFLAGS_DBG) strbyte_sgi.c -o strbyte_sgi_dbg.o


strtermc_sgi.o : strtermc_sgi.c
	cc $(CCFLAGS) strtermc_sgi.c

strtermc_sgi_dbg.o : strtermc_sgi.c
	cc $(CCFLAGS_DBG) strtermc_sgi.c -o strtermc_sgi_dbg.o



#	strnorm is an executable which puts the terminal into "normal"
#	mode, as defined by strterm_norm, and is primarily used after
#	other strterm routines have been called to change the mode of
#	the terminal, and when program execution was terminated before
#	the terminal was reset.

strnorm : strnorm.o ${STR}/libstr.a ${MSG}/libmsg.a
	f77 -o strnorm strnorm.o -L. -L${MSG} -lstr -lmsg -lstr

strnorm.o : strnorm.f
	f77 $(FFLAGS) strnorm.f


strlib_inc : strlib.inc
	ln -s strlib.inc strlib_inc

str_ret_inc : str_ret.inc
	ln -s str_ret.inc str_ret_inc
