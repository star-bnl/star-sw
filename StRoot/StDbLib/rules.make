##############################################################
#
#  Makefile Rules for building the StDbLib (STAR-DB API)
#
#  R.J. Porter, porter@bnl.gov
#
#  options,  gcc w/shared+static lib for linux & solaris
#            CC5 w/shared+static lib for solaris
#            CC4.2 w/shared+static lib & RW-STL for solaris
#
#  options are chosen via tests on 'uname -s' and, if SunOS, on 
#  GNU_GCC & ONL_solaris environment variables 
#
#  many of the flags were taken from Offline build process
#  with only minor investigation.
#
##############################################################

SYSTYPE := $(shell uname -s)

ifndef OPTSTAR
OPTSTAR := /opt/star
endif



ifndef NODEBUG
 DBG = -g 
else
 DBG = -O4
endif


# --> assume egcs
#CC       := /usr/local/bin/gcc
#CXX      := /usr/local/bin/g++
CC       := gcc
CXX      := g++ -std=c++11 -DNoXmlTreeReader

ifeq (SunOS,$(SYSTYPE))

AR       := /usr/ccs/bin/ar -rvu
  OSDEFS   := sun SUN SOLARIS Solaris ST_NO_NUMERIC_LIMITS 
  OSDEFS   += ST_NO_EXCEPTIONS 
  CXXFLAGS := $(DBG) -fPIC -Wall
  CFLAGS   := $(DBG) -fPIC -Wall
  EXEFLAGS := $(DBG) -Wl,-Bdynamic   
  SOFLAGS  := $(DBG) -shared  
  LIBS     := -lnsl -lsocket -lgen

ifndef GNU_GCC
#--> then were using native Sun

  OSDEFS   += ST_NO_MEMBER_TEMPLATES 
  LDFLAGS  :=  $(DBG)  -xar -o
  SOFLAGS  :=  $(DBG)  -G

ifndef ONL_solaris
#-->then we're using CC5

  CC        :=  /opt/WS5.0/bin/cc
  CXX       := /opt/WS5.0/bin/CC
  CXXFLAGS  := $(DBG)  -KPIC 
# +w -features=no%anachronisms -features=rtti 
#  CXXFLAGS  += -library=iostream,no%Cstd 
  CLIBS     := -L/opt/WS5.0/lib -L/opt/WS5.0/SC5.0/lib 
#-liostream 
  CLIBS     += -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb -lmapmalloc

else
#-->CC4.2 with rogue wave

 OSDEFS   += ST_NO_TEMPLATE_DEF_ARGS ONL_solaris ST_NO_NAMESPACES 
 STLHOME  := /online/production/packages/rogue/workspaces/SOLARIS25/SUNPRO42/15d
 CC       := /opt/WS4.0/bin/cc
 CXX      := /opt/WS4.0/bin/CC
 CXXFLAGS := $(DBG)  -KPIC +w -features=no%anachronisms -features=rtti 
 CXXFLAGS += -I/$(STLHOME)/include  
 CLIBS    := -L/opt/WS4.0/lib -L/opt/WS4.0/SC4.2/lib -L$(STLHOME)/lib -lstd15d 
 CLIBS    += -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb -lmapmalloc
 EXTRA_LIBS = -L$(STLHOME)/lib -lstd15d

endif
  AR  := $(CXX) 
else
 OSDEFS   += ST_NO_NAMESPACES
endif
endif


ifeq (Linux,$(SYSTYPE))
# CXX = insure

AR       := /usr/bin/ar -rvu

  OSDEFS     := GNU_GCC ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS LINUX
#  OSDEFS     := GNU_GCC ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS ST_NO_NAMESPACES LINUX
  LD       := $(CXX)
  SO       := $(CXX)
  CXXFLAGS := $(DBG) -fPIC -Wall -I/usr/include/g++ 
  CFLAGS   := $(DBG) -fPIC -Wall
#  LDFLAGS  := $(DBG) -Wl,-Bstatic
  SOFLAGS  := $(DBG) -shared  
  CLIBS    := -L/usr/X11R6/lib  -lXt -lXpm -lX11  -lm -ldl  -rdynamic -lrt

endif

OSDEFS += __STDB_STANDALONE__
LD  := $(CXX)
SO  := $(CXX)


###############################################################
#
# Local includes & libs
#
###############################################################

LIBS += $(EXTRA_LIBS)

CXXFLAGS += $(shell xml2-config --cflags)
CXXFLAGS += $(shell mysql_config --cflags)
#CXXFLAGS += $(shell root-config --cflags)

LOCAL_INCS = -I. -I$(OPTSTAR)/include

SHARED_LIBS = -L$(OPTSTAR)/lib/ -L$(OPTSTAR)/lib/mysql $(LIBS)
SHARED_LIBS += $(shell xml2-config --libs)
SHARED_LIBS += $(shell mysql_config --libs)

#SHARED_LIBS += $(shell root-config --libs)

STATIC_LIBS = $(OPTSTAR)/lib/libmysqlclient.a $(LIBS)

########################################
#
# Gather up the basic set
#
########################################

ALL_CXXFLAGS	= $(CXXFLAGS) 
ALL_CPPFLAGS	= $(CPPFLAGS) $(LOCAL_INCS) 
ALL_LDFLAGS	    = $(LDFLAGS)
ALL_DEFS       := $(addprefix -D,$(OSDEFS))

####################################################
#
#     Basic  Rules
#
#####################################################

.SUFFIXES: .cc .hh .cxx  .h .o

.cxx.o:
	$(CXX) $(ALL_DEFS) $(ALL_CXXFLAGS) $(ALL_CPPFLAGS) -c $*.cxx -o $*.o

.cc.o:
	$(CXX) $(ALL_DEFS) $(ALL_CXXFLAGS) $(ALL_CPPFLAGS) -c $*.cc -o $*.o


clean::
	rm -f core *.o *.bak *.flc
	rm -rf ii_files Templates.DB SunWS_cache












