all:	progs xmltables

#CSTRUCTS= $(STAR)/StRoot/StEEmcDbMaker/cstructs
# fixed path to EEMC header files
CSTRUCTS= $(STAR)/StRoot/StEEmcUtil/database/cstructs


#STDB_API = ../StRoot/StDbLib
STDB_API = ../../../../../StDbLib
include ./rules.make

CXX = /usr/bin/g++

#VPATH=$(CSTRUCTS)

TBLSRC := $(wildcard $(CSTRUCTS)/*Db*.hh)
TBLXML := $(notdir $(patsubst %.hh,%.xml,$(TBLSRC)))
SCHEME := TkEEmcDb

MYSQLINC=-I/usr/include/mysql 
MYSQLLIB=-L/usr/lib64/mysql -lmysqlclient


CXXFLAGS=-g  -fPIC -Wall -I$(STDB_API) -I$(CSTRUCTS) $(MYSQLINC) -DLINUX -D__STDB_STANDALONE__ $(shell xml2-config --cflags)
LDLIBS  = $(STDB_API)/StDbLib.a  $(MYSQLLIB) -lz -lrt $(shell xml2-config --libs)

# use *.C extension for cons to ignore
SRCS =$(wildcard ee*.C EE*.C)

PROGS=eemcDb 


progs:	$(PROGS)

eemcDb: eemcDb.o eemcDbFunctions.o EEmcDbIO.o EEmcDbIOsolo.o 
	$(CXX) -w -O -o $@ $^ $(LDLIBS)


setHVsys: setHVsys.o
	$(CXX) -w -O -o $@ $< $(LDLIBS)

xmltables: $(TBLXML)

clean::
	rm -f $(PROGS) *.o .depend

mrproper: clean
	rm -f $(TBLXML) 


depend .depend : $(SRCS)
	$(CXX) $(CXXFLAGS) -M $(SRCS)  >./.depend

include .depend




