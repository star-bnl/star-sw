#################################################
# $Id: Makefile,v 1.2 2000/02/08 16:35:31 lasiuk Exp $
#################################################
SCL_SYSTEM  = $(STAR_SYS)

SCL_HOME    = /home/star/lasiuk/SCL/StarClassLibrary/

RRS_HOME    = ..

CXXFLAGS    = -I$(RRS_HOME)/include\
              -I$(SCL_HOME)/include\
              -I/opt/aCC/include/SC\
              -I..\
              -g -DRWSTD_BOUNDS_CHECKING -DST_NO_EXCEPTIONS\
              -DST_NO_NAMESPACES


CXX         =  aCC


LD          = aCC -Wl,+vnocompatwarnings +p

LIBS        = -L$(SCL_HOME)/lib/$(STAR_SYS)/ \
              -L$(RRS_HOME)/$(STAR_SYS)/ \
              -L/cern/pro/lib \
              -lStRrs -lscl -lStRrs -lpacklib -lmathlib -l++

DEST        = .

PROGRAM     = maker

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

