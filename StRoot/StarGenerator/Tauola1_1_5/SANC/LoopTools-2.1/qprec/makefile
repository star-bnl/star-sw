Q77 = qprec/q77 -Iqprec

LT = libooptoolsQ.a
LTCHECK = libooptoolsQ_check.a

ALL = $(LT) $(LTCHECK) looptools.h90 qcomplex.mod f77290

all: $(ALL)

install: all
	cd .. ; $(MAKE) LT="qprec/$(LT)" \
	                LTCHECK="qprec/$(LTCHECK)" \
	                LTMMA="" \
	                INCLUDE="qprec/looptools.h90 qprec/qcomplex.mod" \
	                BIN="qprec/q77 qprec/f77290" \
	                install

$(LT): qcomplex.o
	cd .. ; $(MAKE) FC="$(Q77)" \
	                FFC="$(Q77)" \
	                CC="$(Q77)" \
	                OTHEROBJS="qprec/qcomplex.o" \
	                LT="qprec/$(LT)" \
	                qprec/$(LT)

$(LTCHECK): qcomplex.o
	cd .. ; $(MAKE) FC="$(Q77)" \
	                FFC="$(Q77)" \
	                CC="$(Q77)" \
	                OTHEROBJS="qprec/qcomplex.o" \
	                LTCHECK="qprec/$(LTCHECK)" \
	                qprec/$(LTCHECK)

looptools.h90: ../include/looptools.h f77290
	./f77290 ../include/looptools.h looptools.h90

f77290: f77290.c
	$(CC) $(CFLAGS) -o f77290 f77290.c

qcomplex.o qcomplex.mod: qcomplex.f90
	f90 -O -c qcomplex.f90

clean:
	cd .. ; $(MAKE) clean
	rm -f $(ALL) qcomplex.o

