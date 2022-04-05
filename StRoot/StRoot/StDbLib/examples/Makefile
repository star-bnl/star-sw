###########################################
#
#  Simple make file to be used with 
#  the 'rules.make' in StDbLib
#
#  options are:  gcc on linux or solaris
#                CC5 on solaris
#  for solaris, setenv GNU_GCC to use gcc
#
###########################################
#
# this exec
#
ReadConfig = $(THISEXE)
all :: $(ReadConfig)

#
#  set pointer to db-api which include 'rules.make'
#
STDB_API = ../
include $(STDB_API)/rules.make

#
# set pointer to db-cstruct repository
#
#CSTRUCTS= $(STAR)/StDb/include
CSTRUCTS= /online/production/dbAdmin/porter/StDb/include/


ALL_CPPFLAGS += -I$(STDB_API) 
ALL_CPPFLAGS += -I$(CSTRUCTS)
ALL_LIBS = $(STDB_API)/StDbLib.a $(SHARED_LIBS) $(CLIBS)
#-lgen

EXEC_OBJS   =  $(ReadConfig).o 

$(ReadConfig): $(EXEC_OBJS)
	rm -f $@
	$(CXX) -w -O -o $@ $(EXEC_OBJS) $(ALL_LIBS)


clean::
	rm -f $(ReadConfig)

#include .depend
# DO NOT DELETE THIS LINE -- make depend depends on it.






