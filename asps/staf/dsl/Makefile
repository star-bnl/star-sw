
all: dslib dsbin

dslib:
	cd lib; $(MAKE) all

dsbin:
	cd bin; $(MAKE) all

clean:
	cd lib; $(MAKE) clean
	cd bin; $(MAKE) clean


realclean:
	cd bin; $(MAKE) realclean
	cd lib; $(MAKE) realclean
	cd tcp; $(MAKE) clean
tcp:
	cd tcp; $(MAKE) all

vwall: vwlib vwtest

vwlib:
	cd lib; $(MAKE) vwlib

vwtest:
	cd bin; $(MAKE) vwtest

vwclean:
	cd bin; $(MAKE) vwclean
	cd lib; $(MAKE) vwclean
