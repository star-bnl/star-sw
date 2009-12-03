HDRS     = UtilBeamLine3D.h
SRCS     = UtilBeamLine3D.cxx  likeFuncBeamLine3D.cxx

CXX    = g++
LD     = g++
AR     = ar rul

DEBUG  =-g -O 
DEFINES=
#

OBJS   = $(notdir $(patsubst %.cxx,%.o,$(SRCS)))

#
#laptop: CFLAGS   = -Wall $(DEBUG) $(DEFINES) -ansi -I /Applications/root/include/
CFLAGS   =  -m32  -Wall $(DEBUG) $(DEFINES) -ansi -I$(ROOTSYS)/include/

LDFLAGS  =  -m32 -shared 
#laptop: RootLibDir  =/Applications/root/lib/
RootLibDir  =$(ROOTSYS)/lib
RootLibs  =  -lMinuit -lCore  -lPhysics -lHist -lGraf

#-lCint -lGraf3d -lGpad -lTree -lRint -lPostscript -lMatrix  -lm -ldl -rdynamic 

Main =mainFitBeamLine3D

myJob: $(Main).o $(OBJS)
	$(LD) $(LDFLAGS) -o $(Main)  $^  -l m  -L$(RootLibDir) $(RootLibs)

# access to target:  $@

clean:
	-rm -f *.o $(LINK)  $(Main).o $(Main)


depend .depend : $(SRCS)  $(HDRS) 
	$(CXX) -M $(CFLAGS) $^ > ./.depend


%.o: %.cxx
	$(CXX) $(CFLAGS) -c $< -o $@


#
-include .depend
