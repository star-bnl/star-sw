
PROGRAM = read_table_fgt

INCLUDEDIRS = -I../../ `mysql_config --include`

LIBDIRS = -L../../ 

LIBS = `mysql_config --libs` \
	-lxml2 \
	-lrt \
	-lpthread \
	../../StDbLib.a

CXXSOURCES = read_table_fgt.cc

CXXOBJECTS = $(CXXSOURCES:.cpp=.o)  # expands to list of object files
CXXFLAGS = -g `mysql_config --cflags` -Wall $(INCLUDEDIRS) -D__STDB_STANDALONE__
CXX = g++ -std=c++11 -m32

LDFLAGS = $(LIBDIRS) $(LIBS)


all: $(PROGRAM)

$(PROGRAM): $(CXXOBJECTS)
	$(CXX) -o $@ $(CXXOBJECTS) $(CXXFLAGS) $(LDFLAGS)


check:
	cppcheck --enable=all ./*.cpp 2> check_cpp.log

clean:
	$(RM) $(PROGRAM)

run:
	./$(PROGRAM)

