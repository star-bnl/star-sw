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

# --> assume egcs
CC       := /usr/local/bin/gcc
CXX      := /usr/local/bin/g++
AR       := ar -rvu

ifeq (SunOS,$(SYSTYPE))

  OSDEFS   := sun SUN SOLARIS Solaris ST_NO_NUMERIC_LIMITS 
  OSDEFS   += ST_NO_EXCEPTIONS ST_NO_NAMESPACES 
  CXXFLAGS := -g -fPIC -Wall
  CFLAGS   := -g -fPIC -Wall
  EXEFLAGS := -g -Wl,-Bdynamic   
  SOFLAGS  := -g -shared  
  LIBS     := -lnsl -lsocket

ifndef GNU_GCC
#--> then were using native Sun

  OSDEFS   += ST_NO_MEMBER_TEMPLATES
  LDFLAGS  :=  -g  -xar -o
  SOFLAGS  :=  -g  -G

ifndef ONL_solaris
#-->then we're using CC5

  CC        :=  /opt/WS5.0/bin/cc
  CXX       := /opt/WS5.0/bin/CC
  CXXFLAGS  := -g  -KPIC +w -features=no%anachronisms -features=rtti 
  CXXFLAGES += -library=iostream,no%Cstd 
  CLIBS     := -L/opt/WS5.0/lib -L/opt/WS5.0/SC5.0/lib -liostream 
  CLIBS     += -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb -lmapmalloc

else
#-->CC4.2 with rogue wave

 OSDEFS   += ST_NO_TEMPLATE_DEF_ARGS ONL_solaris
 STLHOME  := /online/production/packages/rogue/workspaces/SOLARIS25/SUNPRO42/15d
 CC       := /opt/WS4.0/bin/cc
 CXX      := /opt/WS4.0/bin/CC
 CXXFLAGS := -g  -KPIC +w -features=no%anachronisms -features=rtti 
 CXXFLAGS += -I/$(STLHOME)/include  
 CLIBS    := -L/opt/WS4.0/lib -L/opt/WS4.0/SC4.2/lib -L$(STLHOME)/lib -lstd15d 
 CLIBS    += -lm -lc -L/usr/ucblib -R/usr/ucblib -lucb -lmapmalloc
 EXTRA_LIBS = -L$(STLHOME)/lib -lstd15d

endif
  AR  := $(CXX)
endif
endif

ifeq (Linux,$(SYSTYPE))

  OSDEFS     := GNU_GCC ST_NO_NUMERIC_LIMITS ST_NO_EXCEPTIONS ST_NO_NAMESPACES
  LD       := $(CXX)
  SO       := $(CXX)
  CXXFLAGS := -g -fPIC -Wall -I/usr/include/g++
  CFLAGS   := -g -fPIC -Wall
  LDFLAGS  := -g -Wl,-Bstatic
  SOFLAGS  := -g -shared  
  CLIBS    := -L/usr/X11R6/lib  -lXt -lXpm -lX11  -lm -ldl  -rdynamic 

endif


LD  := $(CXX)
SO  := $(CXX)


###############################################################
#
# Local includes & libs
#
###############################################################

LIBS += $(EXTRA_LIBS)
LOCAL_INCS = -I. -I/opt/star/include
SHARED_LIBS = -L/opt/star/lib/ -L/opt/star/lib/mysql -lmysqlclient $(LIBS)
STATIC_LIBS = /opt/star/lib/libmysqlclient.a $(LIBS)

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












