include ../../make.inc

LIBDIR = $(PREFIX)/lib
INCLUDEDIR = $(PREFIX)/include
BINDIR = $(PREFIX)/bin

LT = libooptools.a
LTCHECK = libooptools_check.a
INCLUDE = include/looptools.h include/clooptools.h include/ltproto.h
BIN = $(LTMMA) utils/ccf utils/F77


all: $(LT) $(LTCHECK) $(LTMMA)

LTCOMMON = lt/C0i.o lt/Cget.o lt/D0i.o lt/Dget.o \
  lt/CA0.o lt/CB0.o lt/CB1.o lt/CB00.o lt/CB11.o \
  lt/CDB0.o lt/CDB00.o lt/CDB1.o lt/CDB11.o \
  lt/CC0.o lt/CD0.o lt/CC0i.o lt/CCget.o lt/CD0i.o lt/CDget.o \
  lt/ini.o lt/cache.o

LTOBJS = lt/A0.o lt/B0.o lt/B1.o lt/B00.o lt/B11.o \
  lt/DB0.o lt/DB00.o lt/DB1.o lt/DB11.o \
  lt/C0.o lt/D0.o $(LTCOMMON)

LTCHECKOBJS = ltcheck/auxB.o ltcheck/auxCD.o ltcheck/check.o \
  ltcheck/A0check.o ltcheck/B0check.o ltcheck/B1check.o \
  ltcheck/B00check.o ltcheck/B11check.o ltcheck/DB0check.o \
  ltcheck/DB1check.o ltcheck/DB00check.o ltcheck/DB11check.o \
  ltcheck/C0check.o ltcheck/D0check.o $(LTCOMMON)

FFOBJS = ff/ffinit.o ff/ffbndc.o ff/ffxli2.o ff/ffcli2.o \
  ff/fftran.o ff/ffabcd.o ff/ff2dl2.o \
  ff/ffcrr.o ff/ffcxr.o ff/ffcxs3.o ff/ffcxs4.o \
  ff/ffxxyz.o ff/ffcxyz.o ff/ffdxc0.o ff/ffdcc0.o ff/ffdcxs.o \
  ff/ffdel2.o ff/ffdel3.o ff/ffdel4.o \
  ff/ffcel2.o ff/ffcel3.o ff/ffcel4.o \
  ff/ffxa0.o ff/ffxb0.o ff/ffxb1.o ff/ffxb2p.o ff/ffxdb0.o ff/ffxdb1.o \
  ff/ffxc0.o ff/ffxc0i.o ff/ffxc0p0.o ff/ffxc0p.o \
  ff/ffxd0.o ff/ffxd0h.o ff/ffxd0i.o ff/ffxd0m0.o ff/ffxd0p.o ff/ffxdbd.o \
  ff/ffca0.o ff/ffcb0.o ff/ffcb1.o ff/ffcb2.o ff/ffcb2p.o ff/ffcdb0.o \
  ff/ffcc0.o ff/ffcc0p.o \
  ff/ffcd0.o ff/ffcdbd.o



.f.o:
	$(F77) -Iff $(FFLAGS) -c $< -o $*.o

.F.o:
	$(F77) -Ilt -Iltcheck -Iff $(FFLAGS) -DQEXT=DBLE -c $< -o $*.o

.c.o:
	$(GCC) $(GCCFLAGS) -c $< -o $*.o


$(LT): $(FFOBJS) $(LTOBJS)
	ar cru $(LT) $(OTHEROBJS) $(LTOBJS) $(FFOBJS)
	-ranlib $(LT)

$(LTCHECK): $(FFOBJS) $(LTCHECKOBJS)
	ar cru $(LTCHECK) $(OTHEROBJS) $(LTCHECKOBJS) $(FFOBJS)
	-ranlib $(LT)

$(FFOBJS): ff/ff.h

$(LTOBJS): ff/ff.h lt/lt.h

$(LTCHECKOBJS): ff/ff.h ltcheck/ltcheck.h


LoopTools: mma/LoopTools.tm $(LT)
	-CC=utils/ccf mcc mma/LoopTools.tm -o LoopTools -Iinclude $(CFLAGS) $(LT)
	-strip LoopTools


clean:
	rm -f $(LT) $(LTCHECK) $(LTMMA) $(LTOBJS) $(LTCHECKOBJS) $(FFOBJS)

