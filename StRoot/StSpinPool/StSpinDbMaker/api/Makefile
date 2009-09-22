

#CSTRUCTS= $(STAR)/StRoot/StEEmcDbMaker/cstructs
CSTRUCTS = ../cstructs
STDB_API = ../../../StDbLib


MYSQLINC=-I/usr/include/mysql
MYSQLLIB=-L/usr/lib/mysql -lmysqlclient -lz

CC:=gcc
CXX:=g++

CXXFLAGS=-g  -fPIC -Wall -I$(STDB_API) -I$(CSTRUCTS) $(MYSQLINC) -DLINUX -D__STDB_STANDALONE__
CXXFLAGS += $(shell xml2-config --cflags)
LDLIBS  = $(STDB_API)/StDbLib.a  $(MYSQLLIB) $(shell xml2-config --libs) -lrt

# use *.C extension for cons to ignore
SRCS =$(wildcard spin*.C Spin*.C )


spinDbAPI: spinDbAPI.o  SpinDbFunctions.o  SpinDbIO.o 
	$(CXX) -w -O -o $@ $^ $(LDLIBS)


clean::
	rm -f spinDbAPI *.o .depend

depend .depend : $(SRCS)
	$(CXX) $(CXXFLAGS) -M $(SRCS)  >./.depend

include .depend




