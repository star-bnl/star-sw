include $(STAR)/asps/staf/MakeArch.mk
CMDS  = atlsim starsim gstar staf staf+ staf++ Staf
SIZE  = 1 2 4 6 8 10 12 14 16 18 20 22 24 28 32 34 36
INCL  = -Iinc  -I$(STAR)/asps/staf/inc -DSTAF
STAF  = YES
ifeq (,$(findstring $(STAF_ARCH),i386_linux2))
CCload = YES
Motif = YES
endif
ifneq (,$(findstring $(STAF_ARCH),sunx86_55))
SHELL = /usr/bin/ksh
GEA   = f77  -g
FOR   = f77  -g
CC    = cc   -g
CPP   = CC   -g -DDEBUG
GST   = f77  -g -t -z muldefs
GSC   = CC   -g -t -z muldefs
FSL   = f77  -w -pic -Nq1500 -Nl100
LDS   = /usr/ccs/bin/ld -G
RMF   = /usr/bin/rm     -f
LIB   = -L/opt/SUNWspro/SC4.2/lib/libp -lM77 -lF77 -lsunmath -lmalloc
LIBSL = -L/opt/SUNWspro/SC4.2/lib/libp -lsunmath
endif
ifneq (,$(findstring $(STAF_ARCH),i386_linux2))
SHELL = /bin/sh
RMF   = /bin/rm  -f
FOR   = pgf77  -w -O2 -fno-second-underscore
GEA   = pgf77  -w -g
# Fedunov: GST   = pgf77 -fPIC ? -fno-underscoring
GST   = pgf77  -w -O2 -export-dynamic -fno-second-underscore
LIB   = -ldl -L/usr/X11R6/lib/ -lX11 -lXt # -lXm 
FSL   = pgf77  -w -O2
LDS   = ld   -ldl -shared
CC    = cc
CPP   = g++
LIBSL =
endif
ifneq (,$(findstring $(STAF_ARCH),sgi_52 sgi_53))
SHELL = /usr/bin/ksh
FOR   = f77  -Nn20000  -Nq20000   -O2
GEA   = f77  -static    -O2
GST   = f77             -O2
FSL   = f77  -Nn20000  -Nq20000   -O2
CC    = cc
CPP   = CC   -g -DDEBUG
LDS   = ld   -shared
GST   = f77             -O2
GSC   = CC   -g -DDEBUG
RMF   = /usr/bin/rm     -f
LIB   = -lftn -lm -lc -lmalloc
LIBSL =
endif
ifneq (,$(findstring $(STAF_ARCH),sgi_64 ))
SHELL = /usr/bin/ksh
FOR   = f77  -32 -Nn20000 -O2
GEA   = f77  -32 -static  -O2
FSL   = f77  -32 -Nn20000 -O2
CC    = cc   -32
CPP   = CC   -32  -g  -DDEBUG
LDS   = ld   -32      -shared
GST   = f77  -32          -O2
GSC   = CC   -32  -g  -DDEBUG
RMF   = /usr/bin/rm     -f
LIB   = -lftn -lm -lc -lmalloc
LIBSL =
endif
ifneq (,$(findstring $(STAF_ARCH),hp_ux102 hp700_ux90))
SHELL = /bin/ksh
GEA   = fort77 +ppu  -K -g  -w
FOR   = fort77 +ppu  +T -g
FSL   = fort77 +ppu  +T -g +z
# in optimized version all tracing is lost !
# FSL = fort77 +ppu  +T    +z +O2
CC    = cc        +z -Aa   -D_HPUX_SOURCE
CPP   = aCC    -w -z +Z -g +DAportable
GST   = fort77 +ppu  +T -g +DAportable  -Wl,-E
GSC   = aCC    -w -z +Z -g +DAportable  -Wl,-E
LIB   =
LDS   = ld     -b
LIBSL =
LDC   = ld     -b
LIBCC =  -L/opt/CC/lib -lCsup -lcxx -lcl -lc
RMF   = /bin/rm   -f
# GSC = aCC    -w -z +Z -g +DAportable  -Wl,-E  -Wl,+FPZVOU
# LIB = /opt/fortran/lib/libU77.a
endif
ifneq (,$(findstring $(STAF_ARCH),alpha_osf1 alpha_osf32c alpha_dux40))
SHELL = /bin/ksh
FOR   = f77 -v -fpe2
GEA   = f77    -fpe2
GST   = f77 -g -D 40000000 -T 20000000 -taso -fpe2
FSL   = f77    -fpe2
CC    = cc
CPP   = cxx -g
LDS   = ld -shared -expect_unresolved "*"
LIBSL =  -lUfor -lfor -lFutil -lm -lm_4sqrt -lots -lc
RMF   = /bin/rm -f
LIB   =
endif
ifneq (,$(findstring $(STAF_ARCH),rs_aix31 rs_aix32 rs_aix41))
SHELL = /bin/ksh
GEA   = xlf -O -q maxmem=-1 -q extname -q source
FSL   = xlf -O -qextname    -qrndsngl
FOR   = xlf -O -q maxmem=-1 -q extname -q source
GST   = xlf -NQ20000 -bnoquiet -bkeepfile:fgsim.o -bkeepfile:agsim.o
CC    = cc  -g
CPP   = xlC -g
LDS   = ld  -bnoentry -bE:$*.exp import.map -bh:8 -T512 -H512
LIBSL = -lxlf90 -lxlf -lm -lc
LIB   =  -lld
RMF   = /bin/rm     -f
FLT   = egrep ' [BAD] '|cut -f1 -d' '|sed -e 's/^#/ #/'|sort|uniq
endif
ifneq (,$(findstring $(STAF_ARCH),sun4x_55 sun4x_56))
SHELL = /bin/ksh
LANG  = /opt/SUNWspro/bin
FOR   = $(LANG)/f77
GEA   = $(LANG)/f77
GST   = $(LANG)/f77
FSL   = $(LANG)/f77 -PIC -Nl100 -Nx1000 -Nq1500
CC    = $(LANG)/cc  -g
CPP   = $(LANG)/CC  -g -DDEBUG
GSC   = $(LANG)/CC  -g -t -z muldefs
LDS   = /usr/ccs/bin/ld -G
LIBSL =
RMF   = /bin/rm     -f
LIB   = -L/usr/dt/lib -L/usr/SUNWspro/lib -ldl  -lM77 -lF77 -lsunmath -lmalloc
endif
load  = $(GST) -g  fgsim.f  ccsim.o
ifdef CCload
load  = $(GSC) -g  ccsim.cc fgsim.o
endif
ifdef STAF
load  += -DSTAF $(INCL)
endif
loadlibs :=
ifdef GCALOR
loadlibs +=     -L/afs/cern.ch/atlas/offline/@sys/pro/lib -lgcalor 
endif
ifdef STAF
 loadlibs +=    -L../lib -ltdm -lspx -lsoc -lasu -ltop -ltnt -lami -ldio -ldui -ldsl -ldsu 
endif
ifdef  Motif
ifdef  Shift
loadlibs +=	`cernlib geant321 pawlib graflib/Motif packlib-shift mathlib kernlib-shift` /usr/local/lib/libshift.a
else
loadlibs +=	`cernlib geant321 pawlib graflib/Motif packlib mathlib kernlib`
endif
else 
ifdef  Shift
loadlibs +=	`cernlib geant321 pawlib graflib/X11  packlib-shift mathlib kernlib-shift` /usr/local/lib/libshift.a
else
 loadlibs +=	`cernlib geant321 pawlib graflib/X11  packlib mathlib kernlib` 
endif
endif

#.SILENT:
.SUFFIXES:
.SUFFIXES: .sl .o .g .f .c .cc .cdf .rz
#
.g.sl:; geant3    $*
	$(FSL) -c $*.f -o  $*.o
ifneq (,$(findstring $(STAF_ARCH),rs_aix31 rs_aix32 rs_aix41))
	nm        $*.o|egrep ' [BAD] '|cut -f1 -d' '|sed -e 's/^#/ #/'|sort|uniq > $*.exp
	$(RMF)    ../sl/$*.sl
#	$(RMF)          $*.sl
endif
	$(LDS)    $*.o -o  $*.sl    $(LIBSL)
	$(RMF)    $*.f     $*.o
#
.f.sl:; $(FSL) -c $*.f -o  $*.o
	$(LDS)    $*.o -o  $*.sl    $(LIBSL)
	$(RMF)             $*.o
#
.c.sl:; $(CC)  -c $*.c -o  $*.o     $(INCL)
ifneq (,$(findstring $(STAF_ARCH),rs_aix31 rs_aix32 rs_aix41))
	nm        $*.o|egrep ' [BAD] '|cut -f1 -d' '|sed -e 's/^#/ #/'|sort|uniq > $*.exp
	$(RMF)    ../sl/$*.sl
#	$(RMF)          $*.sl
endif
	$(LDS)    $*.o -o  $*.sl    $(LIBSL)
	$(RMF)    $*.o
#
.cc.sl:;$(CPP) -c $*.cc -o $*.o     $(INCL)
	$(LDS)    $*.o -o  $*.sl    $(LIBCC)
	$(RMF)    $*.o
#
.cdf.o:;kuipc     $*
	$(FOR) -c $*.f -o  $*.o
	$(RMF)    $*.f
.g.o:;  geant3    $*
	$(FOR) -c $*.f -o  $*.o
	$(RMF)    $*.f
#
.f.o:;  $(FOR) -c $*.f -o  $*.o
.c.o:;  $(CC)  -c $*.c -o  $*.o
.cc.o:; $(CPP) -c $(INCL)  $*.cc -o $*.o
#
qp_name.o: qp_name.c
	 cc -c -g -I/cern/pro/include -I /cern/pro/src/pawlib/paw/ntuple qp_name.c
#
geant3: geant3.f; $(GEA) -o geant3 geant3.f `cernlib kernlib`
#
$(CMDS):ccsim.o fgsim.o agsim.o kgsim.o ggsim.o comisf.o comisc.o qp_name.o agdummy.o
	$(load) agsim.o kgsim.o ggsim.o comisf.o comisc.o agdummy.o qp_name.o\
        $(loadlibs)   $(LIB)  -o $@
#
	echo '#!'$(PWD)'/$@'  > import.map
ifneq (,$(findstring $(STAF_ARCH),rs_aix31 rs_aix32 rs_aix41))
	nm $@|egrep ' [BAD] '|cut -f1 -d' '|sed -e 's/^#/ #/'|sort|uniq>>import.map
	xlf $@ -o $@  -bE:import.map -lX11 -lXm -lXt
endif
#	mv  /tmp/$@  ./$@
 
$(SIZE):
	sed -e '5,15s/8 000/$@ 000/; 5,15s/2 000/500/' fgsim.f > simsize.f
	$(FOR) -c simsize.f -o  fgsim.o
normal:
	$(FOR) -c fgsim.f   -o  fgsim.o
detm:
	$(RMF) detm.rz; echo "createdoc dzdoc.bank detm.rz; quit;" | dzedit
cleanup:
	/bin/rm -f *.o *.f last* *~ *.lst *.exp
	/bin/rm -f agsim.g agdummy.g kgsim.cdf comisc.* ccsim.cc
	/bin/rm -f cmzsave.dat paw.metafile dzedit.las
	ls -ltr
atlas:	geant3 atlsim detm cleanup
star:	geant3 gstar  detm cleanup
setup:
