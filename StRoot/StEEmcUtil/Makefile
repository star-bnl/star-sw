LIBNAME= StEEmcUtil
SUBDIRS= EEevent EEfeeRaw EEmcGeom EEmcSmdMap 
AR     = ar rul
ARX    = ar x
CXX    = g++
LDFLAGS= -shared
LIBDIR = ../../lib

all:	compile lib

compile:	
	for i in $(SUBDIRS); do echo "make all   in $$i" ; cd $$i; $(MAKE) ; cd ..; done

clean:
	for i in $(SUBDIRS); do echo "make clean in $$i" ; cd $$i; $(MAKE) clean ; cd ..; done
	rm -f *.so *.a  *.o

lib:	
	for i in $(SUBDIRS); do $(ARX) $$i/lib$$i.a; done
	$(AR)                lib$(LIBNAME).a  *.o 
	$(CXX) $(LDFLAGS) -o lib$(LIBNAME).so *.o 
	ln -sf lib$(LIBNAME).so $(LIBNAME).so
	rm -f *.o

install:
	mkdir -p        $(LIBDIR)
	cp -pr *.a *.so $(LIBDIR)
