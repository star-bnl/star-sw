
PROGRAM = read_table_fgt

INCLUDEDIRS = -I../../ -I/afs/rhic.bnl.gov/star/packages/DEV/.sl53_gcc432/include

LIBDIRS = -L../../ -L/usr/lib64/mysql

LIBS = -lmysqlclient \
	-lxml2 \
	-lrt \
	../../StDbLib.a

CXXSOURCES = read_table_fgt.cc

CXXOBJECTS = $(CXXSOURCES:.cpp=.o)  # expands to list of object files
CXXFLAGS = -g -Wall $(INCLUDEDIRS) -D__STDB_STANDALONE__
CXX = g++

LDFLAGS = $(LIBDIRS) $(LIBS)


all: $(PROGRAM)


$(PROGRAM): $(CXXOBJECTS)
	$(CXX) -o $@ $(CXXOBJECTS) $(CXXFLAGS) $(LDFLAGS)


check:
	cppcheck --enable=all ./*.cpp 2> check_cpp.log

clean:
	$(RM) -f $(CXXOBJECTS) $(PROGRAM)

run:
	./$(PROGRAM)

