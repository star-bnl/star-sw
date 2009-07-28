.SUFFIXES: .cxx
# Cygwin support
ifeq ($(OS),Windows_NT)
EXE   = .exe
SOEXT = dll
else
EXE   =
SOEXT = so
endif

# CC  = gcc
CC  = g++
CXX = g++
LD  = g++

# Standard definitions
INCDIRS   = . StRoot/StStarLogger/logging $(OPTSTAR)/include $(OPTSTAR)/include/mysql

CFLAGS    = -Wall -c  $(INCDIRS:%=-I%) 
CXXFLAGS  = $(CFLAGS)
# EASYFLAGS = -c  $(INCDIRS:%=-I%) 
LDFLAGS   = -o
SOFLAGS   = -shared -Wl,-Bdynamic -o

# Because it is not defined in my PATH, I need to define by hand
ifeq ($(OS),Windows_NT)
JAVAC= /cygdrive/c/Program\ Files/Java/jdk1.5/jdk/bin/javac.exe
JAVAX= /cygdrive/c/Program\ Files/Java/jdk1.5/jdk/bin/java.exe
JINC= -I/cygdrive/c/Program\ Files/Java/jdk1.5/jdk/include -I/cygdrive/c/Program\ Files/Java/jdk1.5/jdk/include/win32
else
JAVAC=javac
JAVAX=java
JINC= -I/usr/java/latest/include -I/usr/java/latest/include/linux
endif

# My project definition (same name in .i)
MYMOD = logging

UCMINC = $(STAR_USM_MODULE_DIR)/TxEventLog.h 

PERL_INC = $(shell ./perllink.pl)
PERL_LDL = $(shell ./perllink.pl LD)

STAR_USM_BASE_DIR = StRoot/StStarLogger/logging
# targets and sources

# $(MYMOD).cxx


SRCCPPS +=TxEventLogCollector.cpp  \
          TxEventLog.cpp           \
          TxEventLogFactory.cxx    \
          TxEventLogFile.cpp       \
          TxEventLogWeb.cpp        \
          TxUCMCollector.cxx
          
# SRCS += $(SRCCPPS:%=$(STAR_USM_BASE_DIR)/%)

INCS = $(STAR_USM_BASE_DIR)/TxEventLogCollector.h      \
       $(STAR_USM_BASE_DIR)/TxEventLogFactory.h        \
       $(STAR_USM_BASE_DIR)/TxEventLogFile.h           \
       $(STAR_USM_BASE_DIR)/TxEventLog.h               \
       $(STAR_USM_BASE_DIR)/TxEventLogWeb.h            \
       $(STAR_USM_BASE_DIR)/TxUCMCollector.h           \
       $(STAR_USM_BASE_DIR)/TxUCMConstants.h           \
       $(STAR_USM_BASE_DIR)/TxUCMUtils.h

INCSI = $(STAR_USM_BASE_DIR)/TxEventLogFactory.h       \
        $(STAR_USM_BASE_DIR)/TxEventLog.h              \
        $(STAR_USM_BASE_DIR)/TxUCMConstants.h

SWIGI = $(STAR_USM_BASE_DIR)/$(MYMOD).i

OBJS  = $(filter-out %.cxx,$(SRCS:%.cpp=%.o))
OBJS += $(filter-out %.cpp,$(SRCS:%.cxx=%.o))


JAVAS= $(MYMOD).java
JDEPS= $(MYMOD)_java_wrap.cxx
JOBJS= $(MYMOD)_java_wrap.o  $(JSRCS:%.cxx=%.o)

PSRCS= $(MYMOD).java
PDEPS= $(MYMOD)_perl_wrap.cxx
POBJS= $(MYMOD)_perl_wrap.o  $(PSRCS:%.cxx=%.o)

# Main target $(MYMOD).$(SOEXT)  $(MYMOD)_main.class $(MYMOD)_perl.$(SOEXT)
all: lib$(MYMOD)_java.$(SOEXT) lib$(MYMOD).$(SOEXT)
	@ echo "Try % perl test.pl"
	@ echo "Try % $(JAVAX) -Djava.library.path=./ $(MYMOD)_main"

# $(OBJS): $(SRCS)

# the needed .so for the pm to work
lib$(MYMOD).$(SOEXT): $(OBJS)
	$(LD) $(SOFLAGS) $@ $(OBJS)

# the needed .so for the pm to work
lib$(MYMOD)_perl.$(SOEXT): $(POBJS)  lib$(MYMOD).$(SOEXT)
	$(LD) $(SOFLAGS) $@ $(POBJS) $(PERL_LDL)  -L. -llogging

lib$(MYMOD)_java.$(SOEXT): $(JOBJS) lib$(MYMOD).$(SOEXT)
	$(LD) $(SOFLAGS) $@ $(JOBJS) -L. -llogging



#+
# SWIG dependencies
#-

# the creation of the perl module wrappers are made by swig. a .pm will be
# created as well
$(MYMOD)_perl_wrap.cxx: $(SWIGI) $(INCSI)
	swig -perl5 -c++ -o $@ $<

$(MYMOD)_java_wrap.cxx: $(SWIGI) $(INCSI)
	swig $(INCDIRS:%=-I%) -java -c++ -o $@ $<

# XXX_wrap.cxx will be generated from the .i, .i was created
# once in "lazzy" mode. Compile using langage specific
$(MYMOD)_perl_wrap.o: $(MYMOD)_perl_wrap.cxx
	$(CC)  $< -o $@ $(PERL_INC)

$(MYMOD)_java_wrap.o: $(MYMOD)_java_wrap.cxx
	$(CC)  -c $(INCDIRS:%=-I%)   $(JINC) $<

# $(MYMOD)_main.class: test.java $(MYMOD).java $(SRCS:%.cpp=%.java)
$(MYMOD)_main.class:  $(MYMOD).java $(UCMINC:%.h=%.java)
	$(JAVAC) $?


# Global
.c.o:
	$(CC) $(CFLAGS) $< -o $@

.cxx.o:
	$(CXX) $(CXXFLAGS) $< -o $@

.cpp.o:
	$(CXX) $(CXXFLAGS)  $< -o $@

clean:
	/bin/rm -f *.o
	/bin/rm -f *.class
	/bin/rm -f *.java
	/bin/rm -f *_wrap.c
	/bin/rm -f *_wrap.cxx
	/bin/rm -f *~ *.*~
	/bin/rm -f $(MYMOD)*.java
	/bin/rm -f *.$(SOEXT)
	/bin/rm -f $(MYMOD).pm

realclean: clean

print:
	echo $(JOBJS)


