.SUFFIXES: .cxx
# Cygwin support
ifeq ($(OS),Windows_NT)
EXE   = .exe
SOEXT = dll
else
EXE   =
SOEXT = so
endif

X86_DIR = X86
X86_64_DIR = X86_64
OPTSTAR_64 = /opt/star/sl53_x8664_gcc432

CC  = g++ -fPIC -g
CXX = g++ -fPIC -g
LD  = g++ 

#CC  = g++ -fPIC 
#CXX = g++ -fPIC 
#LD  = g++ 
STAR_USM_BASE_DIR = StRoot/logging

# Standard definitions
INCDIRS   = . $(STAR_USM_BASE_DIR)          \
              $(STAR)/StRoot                \
              $(OPTSTAR)/include            \
              /usr/include/mysql
#              $(OPTSTAR)/include/mysql

CFLAGS    = -Wall -c  $(INCDIRS:%=-I%) 
CXXFLAGS  = $(CFLAGS)
# EASYFLAGS = -c  $(INCDIRS:%=-I%) 
LDFLAGS   = -o
SOFLAGS   = -shared -Wl,-Bdynamic -o
DISTTAR   =  ucmdist.tar.gz
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
MYJAR = tx-ucm
MYMAINLOG = ulog

UCMINC = $(STAR_USM_MODULE_DIR)/TxEventLog.h 

PERL_INC = $(shell ./perllink.pl)
PERL_LDL = $(shell ./perllink.pl LD)
# targets and sources

# $(MYMOD).cxx


SRCCPPS +=TxEventLogCollector.cpp  \
          TxEventLog.cpp           \
          TxEventLogFile.cpp       \
          TxEventLogWeb.cpp        \
          StRecord.cxx             \
          StDbFieldI.cxx           \
          TxEventLogFactory.cxx    \
          FieldList.cxx            \
          RecordList.cxx           \
          StDataException.cxx      \
          StDbFieldIIterator.cxx   \
          StRecordIterator.cxx     \
          StUcmEvent.cxx           \
          StUcmEvents.cxx          \
          StUCMException.cxx       \
          StUcmJob.cxx             \
          StUcmJobs.cxx            \
          StUcmTask.cxx            \
          StUcmTasks.cxx           \
          TxUCMCollector.cxx


SRCS += $(SRCCPPS:%=$(STAR_USM_BASE_DIR)/%)

INCSI = $(STAR_USM_BASE_DIR)/TxEventLogFactory.h       \
        $(STAR_USM_BASE_DIR)/TxEventLog.h              \
        $(STAR_USM_BASE_DIR)/StRecord.h                \
        $(STAR_USM_BASE_DIR)/StDbFieldI.h              \
        $(STAR_USM_BASE_DIR)/RecordList.h              \
        $(STAR_USM_BASE_DIR)/StUcmJobs.h               \
        $(STAR_USM_BASE_DIR)/StUcmTasks.h              \
        $(STAR_USM_BASE_DIR)/StUcmEvents.h             \
        $(STAR_USM_BASE_DIR)/StRecordIterator.h        \
        $(STAR_USM_BASE_DIR)/TxUCMConstants.h
        
INCS = $(STAR_USM_BASE_DIR)/TxEventLogCollector.h      \
       $(STAR_USM_BASE_DIR)/TxEventLogFile.h           \
       $(STAR_USM_BASE_DIR)/TxEventLogWeb.h            \
       $(STAR_USM_BASE_DIR)/TxUCMCollector.h           \
       $(STAR_USM_BASE_DIR)/StDbFieldIIterator.h       \
       $(STAR_USM_BASE_DIR)/TxUCMCollector.h           \
       $(STAR_USM_BASE_DIR)/TxUCMUtils.h               \
       $(STAR_USM_BASE_DIR)/FieldList.h                \
       $(STAR_USM_BASE_DIR)/StUcmJob.h                \
       $(STAR_USM_BASE_DIR)/StUcmTask.h               \
       $(STAR_USM_BASE_DIR)/StUcmEvent.h              \
       $(INCSI)

SWIGI = $(STAR_USM_BASE_DIR)/$(MYMOD).i

OBJSTMP  = $(notdir $(filter-out %.cxx,$(SRCS:%.cpp=%.o)))
OBJSTMP += $(notdir $(filter-out %.cpp,$(SRCS:%.cxx=%.o)))

X86_OBJS = $(OBJSTMP:%=$(X86_DIR)/%)
X86_64_OBJS = $(OBJSTMP:%=$(X86_64_DIR)/%)

# JAVASRC = $(STAR_USM_BASE_DIR)/loadlogging.java
JAVAS= $(MYMOD).java
JDEPS= $(MYMOD)_java_wrap.cxx
JOBJS= $(MYMOD)_java_wrap.o  $(JSRCS:%.cxx=%.o)

PSRCS= $(MYMOD).perl
PDEPS= $(MYMOD)_perl_wrap.cxx
POBJS= $(MYMOD)_perl_wrap.o  $(PSRCS:%.cxx=%.o)

# Main target $(MYMOD).$(SOEXT)  $(MYMOD)_main.class $(MYMOD)_perl.$(SOEXT)
# 
all: $(X86_DIR) $(X86_64_DIR) $(X86_DIR)/lib$(MYMOD).$(SOEXT)  $(X86_64_DIR)/lib$(MYMOD).$(SOEXT)  $(MYJAR).jar  $(X86_DIR)/$(MYMAINLOG) $(X86_64_DIR)/$(MYMAINLOG)
	@ echo "Try % perl test.pl"
	@ echo "Try % $(JAVAX) -Djava.library.path=./ $(MYMOD)_main"

$(X86_DIR):
	mkdir $@
   
$(X86_64_DIR): 
	mkdir $@
   
# $(OBJS): $(SRCS)
# make jar
$(MYJAR).jar: $(X86_DIR)/lib$(MYMOD)_java.$(SOEXT)
	javac -d ./ *.java
	jar -cf $@ com

# the needed .so for the pm to work
$(X86_64_DIR)/lib$(MYMOD)Impl.$(SOEXT): $(X86_64_OBJS)
	$(LD) -m64  $(SOFLAGS) $@ $^ -L$(OPTSTAR_64)/lib -llog4cxx -L/usr/lib64/mysql -lmysqlclient 

$(X86_DIR)/lib$(MYMOD)Impl.$(SOEXT): $(X86_OBJS) 
	$(LD) -m32 $(SOFLAGS)  $@ $^ -L$(OPTSTAR)/lib -llog4cxx -L/usr/lib/mysql -lmysqlclient 

# the needed .so for the pm to work
lib$(MYMOD)_perl.$(SOEXT): $(POBJS)  lib$(MYMOD)Impl.$(SOEXT)
	$(LD) $(SOFLAGS) $@ $(POBJS) $(PERL_LDL)  -L. -l$(MYMOD)Impl

$(X86_DIR)/lib$(MYMOD)_java.$(SOEXT): $(JOBJS:%=$(X86_DIR)/%) $(X86_DIR)/lib$(MYMOD)Impl.$(SOEXT) $(X86_DIR)/liblog4cxx.so
	$(LD) -m32 $(SOFLAGS) $@ $(JOBJS:%=$(X86_DIR)/%) -L$(X86_DIR) -l$(MYMOD)Impl

$(X86_64_DIR)/lib$(MYMOD)_java.$(SOEXT):  $(JOBJS:%=$(X86_64_DIR)/%) $(X86_64_DIR)/lib$(MYMOD)Impl.$(SOEXT) $(X86_64_DIR)/liblog4cxx.so
	$(LD) -m64 $(SOFLAGS) $@ $(JOBJS:%=$(X86_64_DIR)/%) -L$(X86_64_DIR) -l$(MYMOD)Impl
        
$(X86_64_DIR)/lib$(MYMOD).$(SOEXT):     $(X86_64_DIR)/lib$(MYMOD)_java.$(SOEXT)   
	cd $(X86_64_DIR) && rm -f lib$(MYMOD).$(SOEXT) && ln -s lib$(MYMOD)_java.$(SOEXT) lib$(MYMOD).$(SOEXT) 

$(X86_DIR)/lib$(MYMOD).$(SOEXT):     $(X86_DIR)/lib$(MYMOD)_java.$(SOEXT)   
	cd $(X86_DIR) && rm -f lib$(MYMOD).$(SOEXT)  && ln -s lib$(MYMOD)_java.$(SOEXT) lib$(MYMOD).$(SOEXT) 


$(X86_DIR)/%.o:  $(STAR_USM_BASE_DIR)/%.cpp
	$(CC) -m32 $(CFLAGS) $< -o $@

$(X86_DIR)/%.o:  $(STAR_USM_BASE_DIR)/%.cxx
	$(CC) -m32 $(CFLAGS) $< -o $@

$(X86_64_DIR)/%.o:  $(STAR_USM_BASE_DIR)/%.cpp
	$(CC) -m64 $(CFLAGS) $< -o $@

$(X86_64_DIR)/%.o:  $(STAR_USM_BASE_DIR)/%.cxx
	$(CC) -m64 $(CFLAGS) $< -o $@
        
setenv.sh: $(STAR_USM_BASE_DIR)/setenv.sh
	cp $< $@ 
	chmod +x $@
#+
# SWIG dependencies
#-

# the creation of the perl module wrappers are made by swig. a .pm will be
# created as well
$(MYMOD)_perl_wrap.cxx: $(SWIGI) $(INCSI)
	swig -perl5 -c++ -o $@ $<

$(MYMOD)_java_wrap.cxx: $(SWIGI) $(INCSI)
	swig $(INCDIRS:%=-I%) -package "com.txLogging" -java -c++ -o $@ $<

# XXX_wrap.cxx will be generated from the .i, .i was created
# once in "lazzy" mode. Compile using langage specific
$(MYMOD)_perl_wrap.o: $(MYMOD)_perl_wrap.cxx
	$(CC)  $< -o $@ $(PERL_INC)

$(X86_DIR)/$(MYMOD)_java_wrap.o: $(MYMOD)_java_wrap.cxx
	$(CC) -m32 -o $(X86_DIR)/$(MYMOD)_java_wrap.o  -c $(INCDIRS:%=-I%)   $(JINC) $<

$(X86_64_DIR)/$(MYMOD)_java_wrap.o: $(MYMOD)_java_wrap.cxx
	$(CC)  -m64 -o $(X86_64_DIR)/$(MYMOD)_java_wrap.o -c $(INCDIRS:%=-I%)   $(JINC) $<
   
# $(MYMOD)_main.class: test.java $(MYMOD).java $(SRCS:%.cpp=%.java)
$(MYMOD)_main.class:  $(MYMOD).java $(UCMINC:%.h=%.java)
	$(JAVAC) $?
        
$(MYMAINLOG).cxx:  $(STAR_USM_BASE_DIR)/TxEventLogFactory.h
	rm -f $@
	echo '#include "$(STAR_USM_BASE_DIR)/TxEventLogFactory.h"' >>$@
	echo 'int main(int argc, const char *argv[]) {   return TxLogging::TxEventLogFactory::main(argc,argv); }' >>$@


$(X86_64_DIR)/$(MYMAINLOG):  $(MYMAINLOG).cxx $(X86_64_DIR)/lib$(MYMOD)Impl.$(SOEXT)
	$(CC) -m64  -o $@ $< $(INCDIRS:%=-I%) -L$(X86_64_DIR) -l$(MYMOD)Impl -L$(OPTSTAR_64)/lib -llog4cxx -L/usr/lib64/mysql -lmysqlclient 


$(X86_DIR)/$(MYMAINLOG): $(MYMAINLOG).cxx $(X86_DIR)/lib$(MYMOD)Impl.$(SOEXT)
	$(CC) -m32  -o $@ $< $(INCDIRS:%=-I%) -L$(X86_DIR) -l$(MYMOD)Impl -L$(OPTSTAR)/lib -llog4cxx -L/usr/lib/mysql -lmysqlclient 

# Global
.c.o:
	$(CC) -m32 $(CFLAGS) $< -o $(X86_DIR)/$@
	$(CC) -m64 $(CFLAGS) $< -o $(X86_64_DIR)/$@

.cxx.o:
	$(CXX) -m32 $(CXXFLAGS) $< -o $(X86_DIR)/$@
	$(CXX) -m64 $(CXXFLAGS) $< -o $(X86_64_DIR)/$@

.cpp.o:
	$(CXX) -m32 $(CXXFLAGS)  $< -o $(X86_DIR)/$@
	$(CXX) -m64 $(CXXFLAGS)  $< -o $(X86_64_DIR)/$@
        
cleanobj:
	/bin/rm -rf $(X86_64_DIR)/*.o  $(X86_DIR)/*.o

clean:
	/bin/rm -rf $(X86_64_DIR)  $(X86_DIR)
	/bin/rm -f *.o
	/bin/rm -rf com
	/bin/rm -f *.java
	/bin/rm -f *_wrap.c
	/bin/rm -f *_wrap.cxx
	/bin/rm -f *~ *.*~
	/bin/rm -f $(MYMOD)*.java
	/bin/rm -f $(MYMOD).pm

distclean: clean
	/bin/rm -f $(MYJAR).jar
	/bin/rm -f *.$(SOEXT)
	/bin/rm -f $(DISTTAR)


print:
	echo $(JOBJS)
        
printsrc:
	@echo $(SRCS)                

printobj:
	@echo $(OBJS)
	@echo X86_OBJS $(X86_OBJS)
	@echo =====================================
	@echo X86_64_OBJS $(X86_64_OBJS)
	@echo =====================================
        
dist:   $(DISTTAR)

$(X86_64_DIR)/liblog4cxx.so:  $(X86_64_DIR) 
	cp -rf /opt/star/sl53_x8664_gcc432/lib/*log4cxx* $(X86_64_DIR)/

$(X86_DIR)/liblog4cxx.so:  $(X86_DIR)
	cp -rf /opt/star/sl53_gcc432/lib/*log4cxx* $(X86_DIR)/

$(DISTTAR):   StRoot/logging/testUcm.java $(MYJAR).jar $(X86_DIR) $(X86_64_DIR) $(X86_64) setenv.sh
	/bin/rm -rf $(X86_64_DIR)/*.o  $(X86_DIR)/*.o
	tar -czvf $@ $^ 
#	tar -czvf ucmdist.tar.gz  --strip-components=2 $? 
        
upload:   $(DISTTAR)  $(X86_64_DIR)/liblog4cxx.so  $(X86_DIR)/liblog4cxx.so
	cp $< ~/WWW/tmp
	tar -tzvf ~/WWW/tmp/$(DISTTAR)


