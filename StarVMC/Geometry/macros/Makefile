# This needs to be changed...
#STAR     = /afs/rhic.bnl.gov/star/packages/eval

output      := .
ARCH        :=  $(STAR_HOST_SYS)

OBJDIR      := .$(STAR_HOST_SYS)/obj
LIBDIR      := .$(STAR_HOST_SYS)/lib

AgMLMODS    := $(wildcard StarVMC/Geometry/*Geo/*.xml) 
AgMLMODC    := $(wildcard StarVMC/Geometry/*Geo/*.xml) $(wildcard StarVMC/Geometry/Compat/*.xml)
AgMLMORT    := $(subst Geometry,xgeometry, $(AgMLMODC))

#AGML_INC    := -I$(STAR) -I`pwd`
AGML_INC    := -I`pwd` -I$(STAR)
ROOT_INC    := -I`root-config --incdir`
INCLUDES    := $(ROOT_INC) $(AGML_INC)

AGC         := agmlParser.py
AGE         := agetof

CXX         := g++ 
CXXFLAGS    := -m32 -g

F77         := gfortran
F77FLAGS    := -m32 -fd-lines-as-code -std=legacy -fno-second-underscore -w -fno-automatic -g

LDFLAGS     := -shared -Wl,-Bdynamic

#                           Number of simultaneous jobs to execute during build
NPROC       := $(shell cat /proc/cpuinfo | grep processor | wc -l)
#NPROC := 1

######################################################################################
#
# Rule to build the STAR Geometry
#
######################################################################################
#
# Main targets:
#
# default: builds the entire AgML system, including the StarGeometry.so and xgeometry.so
#          libraries.  Source files and intermediate files are pushed out into a 
#          directory tree which parallels the StarVMC/Geometry tree.
#
# rootgeom: builds the ROOT geometry (StarGeometry.so) from AgML
#
# agstargeom: builds the AgSTAR geometry (xgeometry.so) from AgML
# 
# clean: removes libraries and the intermediate files under .$(STAR_HOST_SYS)/obj
#
######################################################################################
default:
	@make -j $(NPROC) rootgeom
	@make -j $(NPROC) agstargeom
	@make -j $(NPROC) geometry

rootgeom: $(OBJDIR)/StarVMC/Geometry/StarGeometry.so
	@make -j $(NPROC) $(LIBDIR)/StarGeometry.so     
	@make -j $(NPROC) $(LIBDIR)/libStarGeometry.so  

agstargeom: $(OBJDIR)/StarVMC/xgeometry/xgeometry.so
	@make -j $(NPROC) $(LIBDIR)/xgeometry.so
	@make -j $(NPROC) $(LIBDIR)/libxgeometry.so

geometry: $(OBJDIR)/StarVMC/Geometry/Geometry.so
	@make $(LIBDIR)/Geometry.so
	@make $(LIBDIR)/libGeometry.so

clean:
#	Remove target libraries
	@-rm $(LIBDIR)/StarGeometry.so*
	@-rm $(LIBDIR)/libStarGeometry.so*
	@-rm $(LIBDIR)/xgeometry.so*
	@-rm $(LIBDIR)/libxgeometry.so*
	@-rm $(LIBDIR)/Geometry.so*
	@-rm $(LIBDIR)/libGeometry.so*
#	Remove object and source files
	@-rm -r $(OBJDIR)/StarVMC/Geometry
	@-rm -r $(OBJDIR)/StarVMC/xgeometry


######################################################################################
#
# Rule to build the STAR Geometry
#
######################################################################################


#
# TGeo geometry library
#
# List of modules is the xml file, xml-->o and prefixed by build directory
#

mods         := $(addprefix $(OBJDIR)/, $(patsubst %.xml, %.o,      $(AgMLMODS)))
dicts        := $(addprefix $(OBJDIR)/, $(patsubst %.xml, %_Cint.o, $(AgMLMODS)))
morts        := $(addprefix $(OBJDIR)/, $(patsubst %.xml, %.o,      $(AgMLMORT)))

buildmods: $(mods)

# Number of libraries (expanded when used...)
NLIBS  = $(words $(shell ls $@.*))

$(OBJDIR)/StarVMC/Geometry/StarGeometry.so: $(mods) $(dicts)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $(dicts) $(mods) -o $@.$(NLIBS)
	@sleep 1
	-ln --force $@.$(NLIBS) $@ # Hard link so that Make does not recreate if not necessary

$(OBJDIR)/StarVMC/xgeometry/xgeometry.so: $(morts)
	$(F77) $(F77FLAGS) $(LDFLAGS) $(morts) $(STAR_LIB)/libStarMagFieldNoDict.so -o $@.$(NLIBS)
	@sleep 1
	-ln --force $@.$(NLIBS) $@

$(OBJDIR)/StarVMC/Geometry/Geometry.so: $(OBJDIR)/StarVMC/Geometry/Geometry.o $(OBJDIR)/StarVMC/Geometry/geometryStats.o $(OBJDIR)/StarVMC/Geometry/Geometry_Cint.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@.$(NLIBS)
	@sleep 1
	-ln --force $@.$(NLIBS) $@

$(LIBDIR)/StarGeometry.so: $(OBJDIR)/StarVMC/Geometry/StarGeometry.so
	@mkdir -p $(LIBDIR)
	@sleep 1
	ln -s --force ../../$(OBJDIR)/StarVMC/Geometry/StarGeometry.so $@

$(LIBDIR)/libStarGeometry.so:
	@mkdir -p $(LIBDIR)
	@sleep 1
	ln -s --force ../../$(OBJDIR)/StarVMC/Geometry/StarGeometry.so $@

$(LIBDIR)/xgeometry.so: $(OBJDIR)/StarVMC/xgeometry/xgeometry.so
	@mkdir -p $(LIBDIR)
	@sleep 1
	ln -s --force ../../$(OBJDIR)/StarVMC/xgeometry/xgeometry.so $@

$(LIBDIR)/libxgeometry.so: 
	@mkdir -p $(LIBDIR)
	@sleep 1
	ln -s --force ../../$(OBJDIR)/StarVMC/xgeometry/xgeometry.so $@

$(LIBDIR)/Geometry.so: $(OBJDIR)/StarVMC/Geometry/Geometry.so 
	@mkdir -p $(LIBDIR)
	@sleep 1
	ln -s --force ../../$(OBJDIR)/StarVMC/Geometry/Geometry.so $@

$(LIBDIR)/libGeometry.so: 
	@mkdir -p $(LIBDIR)
	@sleep 1
	ln -s --force ../../$(OBJDIR)/StarVMC/Geometry/Geometry.so $@


#
# xgeometry library is a bit more tricky... need to do pattern subst on path,
# and add in the compatability modules as well.
#
# Also... we should make some of the compatability modules (e.g. xgeometry.g)
# dependent upon the original AgSTAR files.
#


######################################################################################
#
# Rules to build individual geometry modules for TGeo
#
######################################################################################
#
# 1) Copy XML source to $(OBJDIR)/StarVMC/Geometry/MODULE, creating directory
#    path as needed
# 2) Run agmlParser on the XML source to create .cxx and .h files
# 3) Run RootCint.pl on the resulting header files to create a _Cint.cxx file
#    for every module
# 4) Compile the cxx files into object files
#
######################################################################################

# Given an xml file parented on OBJDIR, 
# copy that file into the given directory.
$(OBJDIR)/StarVMC/Geometry/%.xml: StarVMC/Geometry/%.xml
	@mkdir -p $(@D)
	@cp $< $(@D)

# If either a cxx or h file is built, call
# the AgML compiler on the appropriate src
$(OBJDIR)/StarVMC/Geometry/%.cxx $(OBJDIR)/StarVMC/Geometry/%.h:  $(OBJDIR)/StarVMC/Geometry/%.xml
	$(AGC) --export=AgROOT --file=$< --path=$(@D)

# Create the ROOT/Cint file for the corresponding header file.  We do this
# inside of a subdirectory so that there is no interference with parallel
# builds for different modules
$(OBJDIR)/StarVMC/Geometry/%_Cint.cxx: $(OBJDIR)/StarVMC/Geometry/%.h
	@mkdir -p $(@D)/$(basename $(@F))
	@RootCint.pl $(@D)/$(basename $(@F))/$(@F) $< "$(INCLUDES)"
	@mv $(@D)/$(basename $(@F))/$(@F) $@
	@mv $(@D)/$(basename $(@F))/$(basename $(@F)).h $(@D)/$(basename $(@F)).h

# Compile module to an object file
$(OBJDIR)/StarVMC/Geometry/%.o: $(OBJDIR)/StarVMC/Geometry/%.cxx $(OBJDIR)/StarVMC/Geometry/%.h
	$(CXX) $(CXXFLAGS) $(INCLUDES) -I$(@D)/ -c $< -o $@


# Compile dictionary to an object file
$(OBJDIR)/StarVMC/Geometry/%_Cint.o: $(OBJDIR)/StarVMC/Geometry/%_Cint.cxx
	$(CXX) $(CXXFLAGS) $(INCLUDES) -I$(@D)/ -c $< -o $@


######################################################################################
#
# Rules to build individual geometry modules for AgSTAR
#
######################################################################################
#
# 1) Copy XML source as above (into StarVMC/Geometry/...)
# 2) Build the AgSTAR source file from the XML source in StarVMC/Geometry
# 3) Create the fortran file by running the age converter
# 4) Compile the fortran file to create the object file
#
######################################################################################

$(OBJDIR)/StarVMC/xgeometry/%.age: $(OBJDIR)/StarVMC/Geometry/%.xml
	@mkdir -p $(@D)
	$(AGC) --file=$< --export=Mortran > $@

$(OBJDIR)/StarVMC/xgeometry/%.f: $(OBJDIR)/StarVMC/xgeometry/%.age
	$(AGE) $<

$(OBJDIR)/StarVMC/xgeometry/%.o: $(OBJDIR)/StarVMC/xgeometry/%.f
	$(F77) $(F77FLAGS) -c $< -o $@


######################################################################################
#
# Rules to build the Geometry.cxx construction code
#
######################################################################################
$(OBJDIR)/StarVMC/Geometry/Geometry.cxx: StarVMC/Geometry/Geometry.cxx
	@mkdir -p $(@D)
	@cp $< $@
$(OBJDIR)/StarVMC/Geometry/Geometry.h: StarVMC/Geometry/Geometry.h
	@mkdir -p $(@D)
	@cp $< $@
$(OBJDIR)/StarVMC/Geometry/geometryStats.cc: StarVMC/Geometry/geometryStats.cc
	@mkdir -p $(@D)
	@cp $< $@
$(OBJDIR)/StarVMC/Geometry/geometryStats.hh: StarVMC/Geometry/geometryStats.hh
	@mkdir -p $(@D)
	@cp $< $@

$(OBJDIR)/StarVMC/Geometry/Geometry.o: $(OBJDIR)/StarVMC/Geometry/Geometry.cxx $(OBJDIR)/StarVMC/Geometry/Geometry.h $(OBJDIR)/StarVMC/Geometry/geometryStats.o
	$(CXX) $(CXXFLAGS) $(INCLUDES) -I$(@D)/ -c $< -o $@

$(OBJDIR)/StarVMC/Geometry/geometryStats.o: $(OBJDIR)/StarVMC/Geometry/geometryStats.cc $(OBJDIR)/StarVMC/Geometry/geometryStats.hh
	$(CXX) $(CXXFLAGS) $(INCLUDES) -I$(@D)/ -c $< -o $@


# Keep all intermediate files not matter what always dammit
.SECONDARY: