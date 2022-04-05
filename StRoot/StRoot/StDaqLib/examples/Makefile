
INCLUDE = -I../../  -I$(STAR)/StRoot/St_base/ -I$(ROOTSYS)/include

LIBDIR = -L../../../.sl305_gcc323/lib 

client_svt: client_svt.cxx
	g++ $(LIBDIR) $(INCLUDE) client_svt.cxx -o client_svt -lStDaqLib
