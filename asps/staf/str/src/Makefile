ARCH_TAG     = _sgi
ARFLAGS      = vqs
FFLAGS       = -c -O -w -extend_source
FFLAGS_DBG   = -c -g -w -extend_source
CCFLAGS      = -c -O -ansi -fullwarn -v -prototypes
CCFLAGS_DBG  = -c -g -ansi -fullwarn -v -prototypes

all : libstr.a libstr_dbg.a doc

str : libstr.a

dbg : libstr_dbg.a

doc : strlib.f strlib$(ARCH_TAG).f strmth.f strplot.f strterm$(ARCH_TAG).f str_file.f str_parse.f str_time.f
	cd ../doc; docfor ../src/strlib.f;    mv buttons.html strlib.html;    cd ../src
	cd ../doc; docfor ../src/strmth.f;    mv buttons.html strmth.html;    cd ../src
	cd ../doc; docfor ../src/strplot.f;   mv buttons.html strplot.html;   cd ../src
	cd ../doc; docfor ../src/str_file.f;  mv buttons.html str_file.html;  cd ../src
	cd ../doc; docfor ../src/str_parse.f; mv buttons.html str_parse.html; cd ../src
	cd ../doc; docfor ../src/str_time.f;  mv buttons.html str_time.html;  cd ../src
	cd ../doc; docfor ../src/strlib$(ARCH_TAG).f;  mv buttons.html strlib$(ARCH_TAG).html;  cd ../src
	cd ../doc; docfor ../src/strterm$(ARCH_TAG).f; mv buttons.html strterm$(ARCH_TAG).html; cd ../src

libstr.a : strlib.o strlib$(ARCH_TAG).o strmth.o strplot.o strterm$(ARCH_TAG).o str_file.o \
str_parse.o str_time.o \
strbyte$(ARCH_TAG).o strtermc$(ARCH_TAG).o strlibc$(ARCH_TAG).o
	rm -f libstr.a
	ar $(ARFLAGS) libstr.a strlib.o strlib$(ARCH_TAG).o strmth.o strplot.o \
	strterm$(ARCH_TAG).o str_file.o str_parse.o str_time.o \
	strbyte$(ARCH_TAG).o strtermc$(ARCH_TAG).o strlibc$(ARCH_TAG).o
	ranlib libstr.a

libstr_dbg.a : strlib_dbg.o strlib$(ARCH_TAG)_dbg.o strmth_dbg.o strplot_dbg.o strterm$(ARCH_TAG)_dbg.o str_file_dbg.o \
str_parse_dbg.o str_time_dbg.o \
strbyte$(ARCH_TAG)_dbg.o strtermc$(ARCH_TAG)_dbg.o strlibc$(ARCH_TAG)_dbg.o
	rm -f libstr_dbg.a
	ar $(ARFLAGS) libstr_dbg.a strlib_dbg.o strlib$(ARCH_TAG)_dbg.o strmth_dbg.o strplot_dbg.o \
	strterm$(ARCH_TAG)_dbg.o str_file_dbg.o str_parse_dbg.o str_time_dbg.o \
	strbyte$(ARCH_TAG)_dbg.o strtermc$(ARCH_TAG)_dbg.o strlibc$(ARCH_TAG)_dbg.o
	ranlib libstr_dbg.a


strlib.o : strlib.f
	f77 $(FFLAGS) strlib.f

strlib_dbg.o : strlib.f
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


str_file.o : str_file.f str_file_inc
	f77 $(FFLAGS) str_file.f

str_file_dbg.o : str_file.f str_file_inc
	f77 $(FFLAGS_DBG) str_file.f -o str_file_dbg.o


str_parse.o : str_parse.f str_parse_inc
	f77 $(FFLAGS) str_parse.f

str_parse_dbg.o : str_parse.f str_parse_inc
	f77 $(FFLAGS_DBG) str_parse.f -o str_parse_dbg.o


str_time.o : str_time.f
	f77 $(FFLAGS) str_time.f

str_time_dbg.o : str_time.f
	f77 $(FFLAGS_DBG) str_time.f -o str_time_dbg.o


strbyte$(ARCH_TAG).o : strbyte$(ARCH_TAG).c
	cc $(CCFLAGS) strbyte$(ARCH_TAG).c

strbyte$(ARCH_TAG)_dbg.o : strbyte$(ARCH_TAG).c
	cc $(CCFLAGS_DBG) strbyte$(ARCH_TAG).c -o strbyte$(ARCH_TAG)_dbg.o


strtermc$(ARCH_TAG).o : strtermc$(ARCH_TAG).c
	cc $(CCFLAGS) strtermc$(ARCH_TAG).c

strtermc$(ARCH_TAG)_dbg.o : strtermc$(ARCH_TAG).c
	cc $(CCFLAGS_DBG) strtermc$(ARCH_TAG).c -o strtermc$(ARCH_TAG)_dbg.o


strlibc$(ARCH_TAG).o : strlibc$(ARCH_TAG).c
	cc $(CCFLAGS) strlibc$(ARCH_TAG).c

strlibc$(ARCH_TAG)_dbg.o : strlibc$(ARCH_TAG).c
	cc $(CCFLAGS_DBG) strlibc$(ARCH_TAG).c -o strlibc$(ARCH_TAG)_dbg.o



str_file_inc : str_file.inc
	ln -s str_file.inc str_file_inc

str_parse_inc : str_parse.inc
	ln -s str_parse.inc str_parse_inc


strterm_sun.f : strterm_sgi.f
	rm -f strterm_sun.f
	ln -s strterm_sgi.f strterm_sun.f

strtermc_sun.c : strtermc_sgi.c
	rm -f strtermc_sun.c
	ln -s strtermc_sgi.c strtermc_sun.c

strbyte_sun.c : strbyte_sgi.c
	rm -f strbyte_sun.c
	ln -s strbyte_sgi.c strbyte_sun.c
