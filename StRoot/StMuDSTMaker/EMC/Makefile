#-----------------------------------------------------------------------
# Created    20-dec-2000 by PAI
# The goal is creating of shared library without cons
# Corrected   15-feb-2001 by PAI
#-----------------------------------------------------------------------
CXXFLAGS      = -O -I$(ROOTSYS)/include
LDFLAGS       = -g
LD            = gcc
SOFLAGS       = -shared

#   Linux with egcs
CXX           = gcc
CXXFLAGS      = -g 

#   Include directory 
CXXINCLUDE    += -I$(ROOTSYS)/include

NAME1 = StEmcMicroEvent
NAME2 = StEmcMicroTrack
NAME3 = StEmcMicroCollection
NAME4 = StEmcMicroHit
NAME5 = StEmcMicroCluster
NAME6 = StEmcMicroPoint
NAME7 = StFpdMicroCollection

LIB   = $(NAME1)

OBJS          = $(NAME1).o $(NAME1)dict.o\
		$(NAME2).o \
		$(NAME3).o \
		$(NAME4).o \
		$(NAME5).o \
		$(NAME6).o \
		$(NAME7).o

HEADERS       = $(NAME1).h \
		$(NAME2).h \
		$(NAME3).h \
		$(NAME4).h \
		$(NAME5).h \
		$(NAME6).h \
		$(NAME7).h


PROGRAM       = $(LIB).so

#------------------------------------------------------------------------
StEmcMicroEvent.so	:     $(OBJS)              

		@echo "<I> Creating StEmcMicroEvent.so  ..."
		/bin/rm -f $(PROGRAM)
		$(LD) $(SOFLAGS) $(LDFLAGS) $(OBJS) -o $(PROGRAM)
##		cp mylib.so ../new/tbtlib.so
##		chmod 555 mylib.so
		@echo "done"

clean:
		@echo "<I> Clean up before rebuilding ..."
		@rm -f $(PROGRAM) *.o *dict.*

cleandict:
		@echo "<I> Clean up before rebuilding ..."
		@rm -f $(PROGRAM) *dict.*

#
##
#
$(NAME1).o:	$(NAME1).h $(NAME1).cxx
$(NAME2).o:	$(NAME2).h $(NAME2).cxx
$(NAME3).o:	$(NAME3).h $(NAME3).cxx
$(NAME4).o:	$(NAME4).h $(NAME4).cxx
$(NAME5).o:	$(NAME5).h $(NAME5).cxx
$(NAME6).o:	$(NAME6).h $(NAME6).cxx
$(NAME7).o:	$(NAME7).h $(NAME7).cxx

#

$(NAME1)dict.cxx:	$(HEADERS) 
	@echo "<I> Generating cint dictionary for directory "$(NAME1)
	rootcint -f $(NAME1)dict.cxx -c \
        $(CXXFLAGS) $(CXXINCLUDE) \
        $(HEADERS)
         
# -----------------------------------------------------------------
.SUFFIXES: .cxx
#------------------------------------------------------------------------
.cxx.o:
		$(CXX) $(CXXFLAGS) $(CXXINCLUDE) -c $<
