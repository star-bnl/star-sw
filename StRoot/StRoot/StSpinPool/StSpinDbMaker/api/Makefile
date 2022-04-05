

#CSTRUCTS= $(STAR)/StRoot/StEEmcDbMaker/cstructs
CSTRUCTS = ../cstructs
STDB_API = ../../../StDbLib

ifeq ( $(USE_64BITS),1)
   CXXFLAGS =  -m64 -fPIC 
   LDFLAGS  =  -m64
else
   CXXFLAGS =  -m32 -fPIC 
   LDFLAGS  =  -m32
endif

MYSQLINC= $(shell mysql_config --include)
MYSQLLIB= $(shell mysql_config --libs)
XML2FLAG= $(shell xml2-config --cflags)
XML2LIBS= $(shell xml2-config --libs) 

CC:=gcc
CXX:=g++

CXXFLAGS +=-g  -fPIC -Wall -I$(STDB_API) -I$(CSTRUCTS) $(MYSQLINC) -DLINUX -D__STDB_STANDALONE__
CXXFLAGS += $(XML2FLAG)
LDLIBS   += $(STDB_API)/StDbLib.a  $(MYSQLLIB) $(XML2LIBS) -lrt

# use *.C extension for cons to ignore
SRCS =$(wildcard spin*.C Spin*.C )


spinDbAPI: spinDbAPI.o  SpinDbFunctions.o  SpinDbIO.o 
	$(CXX) -w -O $(LDFLAGS) -o $@ $^ $(LDLIBS) 


clean::
	rm -f spinDbAPI *.o .depend

depend .depend : $(SRCS)
	$(CXX) $(CXXFLAGS) -M $(SRCS)  >./.depend

include .depend




