ARCH_TAG     = _sgi
ARFLAGS      = vqs
ARTS         = ar -t
FF           = f77
FFLAGS       = -c -O -w -extend_source
FFLAGS4      = -c -O -w -extend_source -irix4
FFLAGS_DBG   = -c -g -w -extend_source
CC           = cc
CCFLAGS      = -c -O -ansi -fullwarn -prototypes
CCFLAGS4     = -c -O -ansi -fullwarn -prototypes -irix4
CCFLAGS_DBG  = -c -g -ansi -fullwarn -prototypes
FFCC_LINK_FLAGS = 

all : libstr.a libstr_dbg.a

str4 : libstr4.a

str : libstr.a

dbg : libstr_dbg.a

doc : strbyte$(ARCH_TAG).c strlibc$(ARCH_TAG).c strtermc$(ARCH_TAG).c \
strlib.f strlib$(ARCH_TAG).f strmth.f strplot.f strterm$(ARCH_TAG).f str_file.f str_parse.f str_time.f
	docc   strbyte$(ARCH_TAG).c;   mv buttons.html strbyte$(ARCH_TAG).html
	docc   strtermc$(ARCH_TAG).c;  mv buttons.html strtermc$(ARCH_TAG).html
	docfor strlib.f;    mv buttons.html strlib.html
	docfor strmth.f;    mv buttons.html strmth.html
	docfor strplot.f;   mv buttons.html strplot.html
	docfor str_file.f;  mv buttons.html str_file.html
	docfor str_parse.f; mv buttons.html str_parse.html
	docfor str_time.f;  mv buttons.html str_time.html
	docfor strlib$(ARCH_TAG).f;  mv buttons.html strlib$(ARCH_TAG).html
	docfor strterm$(ARCH_TAG).f; mv buttons.html strterm$(ARCH_TAG).html
	mv *.html ../doc

libstr.a : strlib.o strlib$(ARCH_TAG).o strmth.o strplot.o strterm$(ARCH_TAG).o str_file.o \
str_parse.o str_time.o \
strbyte$(ARCH_TAG).o strtermc$(ARCH_TAG).o strlibc$(ARCH_TAG).o
	echo "Making libstr.a"
	rm -f libstr.a
	ar $(ARFLAGS) libstr.a strlib.o strlib$(ARCH_TAG).o strmth.o strplot.o \
	strterm$(ARCH_TAG).o str_file.o str_parse.o str_time.o \
	strbyte$(ARCH_TAG).o strtermc$(ARCH_TAG).o strlibc$(ARCH_TAG).o
	$(ARTS) libstr.a

libstr4.a : strlib.o4 strlib$(ARCH_TAG).o4 strmth.o4 strplot.o4 strterm$(ARCH_TAG).o4 str_file.o4 \
str_parse.o4 str_time.o4 \
strbyte$(ARCH_TAG).o4 strtermc$(ARCH_TAG).o4 strlibc$(ARCH_TAG).o4
	rm -f libstr4.a
	ar $(ARFLAGS) libstr4.a strlib.o4 strlib$(ARCH_TAG).o4 strmth.o4 strplot.o4 \
	strterm$(ARCH_TAG).o4 str_file.o4 str_parse.o4 str_time.o4 \
	strbyte$(ARCH_TAG).o4 strtermc$(ARCH_TAG).o4 strlibc$(ARCH_TAG).o4
	$(ARTS) libstr4.a

libstr_dbg.a : strlib_dbg.o strlib$(ARCH_TAG)_dbg.o strmth_dbg.o strplot_dbg.o strterm$(ARCH_TAG)_dbg.o str_file_dbg.o \
str_parse_dbg.o str_time_dbg.o \
strbyte$(ARCH_TAG)_dbg.o strtermc$(ARCH_TAG)_dbg.o strlibc$(ARCH_TAG)_dbg.o
	rm -f libstr_dbg.a
	ar $(ARFLAGS) libstr_dbg.a strlib_dbg.o strlib$(ARCH_TAG)_dbg.o strmth_dbg.o strplot_dbg.o \
	strterm$(ARCH_TAG)_dbg.o str_file_dbg.o str_parse_dbg.o str_time_dbg.o \
	strbyte$(ARCH_TAG)_dbg.o strtermc$(ARCH_TAG)_dbg.o strlibc$(ARCH_TAG)_dbg.o
	$(ARTS) libstr_dbg.a


strlib.o4 : strlib.f
	$(FF) $(FFLAGS4) strlib.f -o strlib.o4

strlib.o : strlib.f
	$(FF) $(FFLAGS) strlib.f

strlib_dbg.o : strlib.f
	$(FF) $(FFLAGS_DBG) strlib.f -o strlib_dbg.o


strlib$(ARCH_TAG).o4 : strlib$(ARCH_TAG).f str_time_inc
	$(FF) $(FFLAGS4) strlib$(ARCH_TAG).f -o strlib$(ARCH_TAG).o4

strlib$(ARCH_TAG).o : strlib$(ARCH_TAG).f str_time_inc
	$(FF) $(FFLAGS) strlib$(ARCH_TAG).f

strlib$(ARCH_TAG)_dbg.o : strlib$(ARCH_TAG).f str_time_inc
	$(FF) $(FFLAGS_DBG) strlib$(ARCH_TAG).f -o strlib$(ARCH_TAG)_dbg.o


strmth.o4 : strmth.f
	$(FF) $(FFLAGS4) strmth.f -o strmth.o4

strmth.o : strmth.f
	$(FF) $(FFLAGS) strmth.f

strmth_dbg.o : strmth.f
	$(FF) $(FFLAGS_DBG) strmth.f -o strmth_dbg.o


strplot.o4 : strplot.f
	$(FF) $(FFLAGS4) strplot.f -o strplot.o4

strplot.o : strplot.f
	$(FF) $(FFLAGS) strplot.f

strplot_dbg.o : strplot.f
	$(FF) $(FFLAGS_DBG) strplot.f -o strplot_dbg.o


strterm$(ARCH_TAG).o4 : strterm$(ARCH_TAG).f
	$(FF) $(FFLAGS4) strterm$(ARCH_TAG).f -o strterm$(ARCH_TAG).o4

strterm$(ARCH_TAG).o : strterm$(ARCH_TAG).f
	$(FF) $(FFLAGS) strterm$(ARCH_TAG).f

strterm$(ARCH_TAG)_dbg.o : strterm$(ARCH_TAG).f
	$(FF) $(FFLAGS_DBG) strterm$(ARCH_TAG).f -o strterm$(ARCH_TAG)_dbg.o


str_file.o4 : str_file.f str_file_inc
	$(FF) $(FFLAGS4) str_file.f -o str_file.o4

str_file.o : str_file.f str_file_inc
	$(FF) $(FFLAGS) str_file.f

str_file_dbg.o : str_file.f str_file_inc
	$(FF) $(FFLAGS_DBG) str_file.f -o str_file_dbg.o


str_parse.o4 : str_parse.f str_parse_inc
	$(FF) $(FFLAGS4) str_parse.f -o str_parse.o4

str_parse.o : str_parse.f str_parse_inc
	$(FF) $(FFLAGS) str_parse.f

str_parse_dbg.o : str_parse.f str_parse_inc
	$(FF) $(FFLAGS_DBG) str_parse.f -o str_parse_dbg.o


str_time.o4 : str_time.f
	$(FF) $(FFLAGS4) str_time.f -o str_time.o4

str_time.o : str_time.f
	$(FF) $(FFLAGS) str_time.f

str_time_dbg.o : str_time.f
	$(FF) $(FFLAGS_DBG) str_time.f -o str_time_dbg.o


strbyte$(ARCH_TAG).o4 : strbyte$(ARCH_TAG).c
	echo $(ARCH_TAG)
	$(CC) $(CCFLAGS4) strbyte$(ARCH_TAG).c -o strbyte$(ARCH_TAG).o4

strbyte$(ARCH_TAG).o : strbyte$(ARCH_TAG).c
	$(CC) $(CCFLAGS) strbyte$(ARCH_TAG).c

strbyte$(ARCH_TAG)_dbg.o : strbyte$(ARCH_TAG).c
	echo $(ARCH_TAG)
	$(CC) $(CCFLAGS_DBG) strbyte$(ARCH_TAG).c -o strbyte$(ARCH_TAG)_dbg.o


strtermc$(ARCH_TAG).o4 : strtermc$(ARCH_TAG).c
	$(CC) $(CCFLAGS4) strtermc$(ARCH_TAG).c -o strtermc$(ARCH_TAG).o4

strtermc$(ARCH_TAG).o : strtermc$(ARCH_TAG).c
	$(CC) $(CCFLAGS) strtermc$(ARCH_TAG).c

strtermc$(ARCH_TAG)_dbg.o : strtermc$(ARCH_TAG).c
	$(CC) $(CCFLAGS_DBG) strtermc$(ARCH_TAG).c -o strtermc$(ARCH_TAG)_dbg.o


strlibc$(ARCH_TAG).o4 : strlibc$(ARCH_TAG).c
	$(CC) $(CCFLAGS4) strlibc$(ARCH_TAG).c -o strlibc$(ARCH_TAG).o4

strlibc$(ARCH_TAG).o : strlibc$(ARCH_TAG).c
	$(CC) $(CCFLAGS) strlibc$(ARCH_TAG).c

strlibc$(ARCH_TAG)_dbg.o : strlibc$(ARCH_TAG).c
	$(CC) $(CCFLAGS_DBG) strlibc$(ARCH_TAG).c -o strlibc$(ARCH_TAG)_dbg.o



str_file_inc : str_file.inc
	rm -f str_file_inc
	ln -s str_file.inc str_file_inc

str_parse_inc : str_parse.inc
	rm -f str_parse_inc
	ln -s str_parse.inc str_parse_inc

str_time_inc : str_time.inc
	rm -f str_time_inc
	ln -s str_time.inc str_time_inc

strterm_vms_inc : strterm_vms.inc
	rm -f strterm_vms_inc
	ln -s strterm_vms.inc strterm_vms_inc


strterm_sun.f : strterm_sgi.f
	rm -f strterm_sun.f
	ln -s strterm_sgi.f strterm_sun.f

strtermc_sun.c : strtermc_sgi.c
	rm -f strtermc_sun.c
	ln -s strtermc_sgi.c strtermc_sun.c

strbyte_sun.c : strbyte_sgi.c
	rm -f strbyte_sun.c
	ln -s strbyte_sgi.c strbyte_sun.c


strterm_aix.f : strterm_sgi.f
	rm -f strterm_aix.f
	ln -s strterm_sgi.f strterm_aix.f

strtermc_aix.c : strtermc_sgi.c
	rm -f strtermc_aix.c
	ln -s strtermc_sgi.c strtermc_aix.c

strbyte_aix.c : strbyte_sgi.c
	rm -f strbyte_aix.c
	ln -s strbyte_sgi.c strbyte_aix.c


strterm_hpu.f : strterm_sgi.f
	rm -f strterm_hpu.f
	ln -s strterm_sgi.f strterm_hpu.f

strbyte_hpu.c : strbyte_sgi.c
	rm -f strbyte_hpu.c
	ln -s strbyte_sgi.c strbyte_hpu.c
