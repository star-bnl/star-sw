LIBNAME=EEmcUtil
SUBDIRS=EEevent EEfeeRaw EEmcGeom 
AR     = ar rul
ARX    = ar x
CXX    = g++
LDFLAGS= -shared

all:	compile lib

compile:	
	for i in $(SUBDIRS); do echo "make all   in $$i" ; cd $$i; $(MAKE) ; cd ..; done

clean:
	for i in $(SUBDIRS); do echo "make clean in $$i" ; cd $$i; $(MAKE) clean ; cd ..; done
	rm -f lib$(LIBNAME).*  *.o

lib:	
	for i in $(SUBDIRS); do $(ARX) $$i/lib$$i.a; done
	$(AR)               lib$(LIBNAME).a  *.o 
	$(CXX) $(LDFLAGS) -o lib$(LIBNAME).so *.o 
	rm -f *.o
