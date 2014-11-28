CXX           =
ObjSuf        = o
SrcSuf        = cxx
ExeSuf        =
DllSuf        = so
HeadSuf       = h
OutPutOpt     = -o 

DLIB          = $(TESTLIB)

ROOTCFLAGS   := $(shell root-config --cflags)
ROOTLIBS     := $(shell root-config --libs)
ROOTGLIBS    := $(shell root-config --glibs)


CXX           = g++
CXXFLAGS      = -g -O -Wall -fPIC -D__ROOT__ 
LD            = g++
LDFLAGS       = -O
SOFLAGS       = -shared



CXXFLAGS     += $(ROOTCFLAGS)
LIBS          = $(ROOTLIBS) $(SYSLIBS)
GLIBS         = $(ROOTGLIBS) $(SYSLIBS)

#------------------------------------------------------------------------------
TESTOBJECT        = dFitter3d.$(ObjSuf)  dProjector.$(ObjSuf) mydict.o
#TESTSOURCE        = test.$(SrcSuf) 
TESTLIB           = libfit.$(DllSuf)

#------------------------------------------------------------------------------
.SUFFIXES: .$(SrcSuf) .$(ObjSuf) .$(DllSuf) .$(HeadSuf)


#------------------------------------------------------------------------------
$(TESTLIB):     $(TESTOBJECT) mydict.C
		@echo "Linking :"
	        $(LD) $(SOFLAGS) $(LDFLAGS) $(TESTOBJECT) $(OutPutOpt) $(TESTLIB)	
		@echo "$@ created."


.$(HeadSuf).$(SrcSuf): 
	@echo "$? header changed."

.$(SrcSuf).$(ObjSuf):
	@echo "$? will be compiled. "
	$(CXX) $(CXXFLAGS) -c $<


#-----------------------
# rootcint rotz
#-----------------------
mydict.C: dFitter3d.h  dProjector.h
	@echo "Generating dictionary !"
	@rootcint -f mydict.C -c -D__ROOT__ dFitter3d.h dProjector.h LinkDef.h

#------------------------
# special targets
#------------------------
clean:
		@rm -f $(TESTOBJECT) $(TESTLIB) mydict.h mydict.C mydict.o $core