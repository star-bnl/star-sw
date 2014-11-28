# 16-jan-04
#-----------------------------------------------------------------------
LDFLAGS       = -g
LD            = g++
# 12-Aug-2002 - get from cons
SOFLAGS       = -shared -Wl,-Bdynamic
#   Linux with egcs
CXX           = g++
#   Do not optimize -> 31-aug
CXXFLAGS      = -g -O0
#   Include directory 
CXXINCLUDE     = -I$(ROOTSYS)/include
#
NAME1    = StTab
LIBC     = tab
NAMELINK = LinkDefClassForTab.h
#
HEADERS       = $(NAME1).h
OBJS          = $(NAME1).o $(NAME1)dict.o
LIBCSO        = $(LIBC).so
#
$(LIBCSO)	:     $(OBJS)
		@echo "<I> Creating $(LIBCSO) ..."
		/bin/rm -f $(LIBCSO)
		$(LD) $(SOFLAGS) $(LDFLAGS) $(OBJS) -o $(LIBCSO)
		@echo "done"
##
$(NAME1)dict.cxx:	$(HEADERS) $(NAMELINK)
	@echo "<I> Generating cint dictionary for classes "$(HEADERS)
	rootcint -f $(NAME1)dict.cxx -c $(CXXINCLUDE) $(HEADERS) $(NAMELINK)
#
$(NAME1).o:	$(NAME1).h $(NAME1).cxx
# -----------------------------------------------------------------
.SUFFIXES: .cxx
#------------------------------------------------------------------------
.cxx.o:
		$(CXX) $(CXXFLAGS) $(CXXINCLUDE) -c $<
