#################################################
# $Id: Makefile,v 2.0 2000/08/09 16:10:28 gans Exp $
#################################################
SCL_SYSTEM  = $(STAR_SYS)

SCL_HOME    = /home/star/lasiuk/scl/StRoot/StarClassLibrary

RRS_HOME    = ..

CXXFLAGS    = -I$(RRS_HOME)/include\
              -I$(SCL_HOME)\
              -I/opt/aCC/include/SC\
              -I..\
              -g -DRWSTD_BOUNDS_CHECKING -DST_NO_EXCEPTIONS\
              -DST_NO_NAMESPACES


CXX         =  aCC


LD          = aCC -Wl,+vnocompatwarnings +p

LIBS        = -L$(SCL_HOME)/$(STAR_SYS)/ \
              -L$(RRS_HOME)/$(STAR_SYS)/ \
              -L/cern/pro/lib \
              -lStRrs -lStscl -lStRrs -lpacklib -lmathlib -l++

DEST        = .

PROGRAM     = cf

OBJS        = $(PROGRAM).o

SRCS        = $(PROGRAM).cc

all:	$(PROGRAM)

$(PROGRAM):	$(OBJS)
		@echo "Linking $(PROGRAM) ..."
		$(LD) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:
		rm -f $(PROGRAM).o
		rm -f $(PROGRAM)
		rm -f last.kumac
		rm -f core

