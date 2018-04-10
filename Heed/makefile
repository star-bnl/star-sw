OBJDIR = $(GARFIELD_HOME)/Object
SRCDIR = $(GARFIELD_HOME)/Source
INCDIR = $(GARFIELD_HOME)/Include
LIBDIR = $(GARFIELD_HOME)/Library
HEEDDIR = $(GARFIELD_HOME)/Heed

HEEDOBJS = \
	$(OBJDIR)/Heed/FunNameStack.o \
	$(OBJDIR)/Heed/definp.o \
	$(OBJDIR)/Heed/findmark.o \
	$(OBJDIR)/Heed/prstream.o \
	$(OBJDIR)/Heed/AbsArr.o \
	$(OBJDIR)/Heed/AbsPtr.o \
	$(OBJDIR)/Heed/chisran.o \
	$(OBJDIR)/Heed/PointsRan.o \
	$(OBJDIR)/Heed/pois.o \
	$(OBJDIR)/Heed/rnorm.o \
	$(OBJDIR)/Heed/eparticle.o \
	$(OBJDIR)/Heed/particle_def.o \
	$(OBJDIR)/Heed/AtomDef.o \
	$(OBJDIR)/Heed/GasDef.o \
	$(OBJDIR)/Heed/MatterDef.o \
	$(OBJDIR)/Heed/MoleculeDef.o \
	$(OBJDIR)/Heed/inverse.o \
	$(OBJDIR)/Heed/multiply.o \
	$(OBJDIR)/Heed/cubic.o \
	$(OBJDIR)/Heed/DoubleAc.o \
	$(OBJDIR)/Heed/kinem.o \
	$(OBJDIR)/Heed/linexi2.o \
	$(OBJDIR)/Heed/lorgamma.o \
	$(OBJDIR)/Heed/parabol.o \
	$(OBJDIR)/Heed/PolLeg.o \
	$(OBJDIR)/Heed/bethe_bloch.o \
	$(OBJDIR)/Heed/e_cont_enloss.o \
	$(OBJDIR)/Heed/box.o \
	$(OBJDIR)/Heed/circumf.o \
	$(OBJDIR)/Heed/gparticle.o \
	$(OBJDIR)/Heed/mparticle.o \
	$(OBJDIR)/Heed/plane.o \
	$(OBJDIR)/Heed/polyline.o \
	$(OBJDIR)/Heed/straight.o \
	$(OBJDIR)/Heed/surface.o \
	$(OBJDIR)/Heed/trajestep.o \
	$(OBJDIR)/Heed/vec.o \
	$(OBJDIR)/Heed/volume.o

HEEDOBJS += \
	$(OBJDIR)/Heed/BGMesh.o \
	$(OBJDIR)/Heed/ElElasticScat.o \
	$(OBJDIR)/Heed/EnergyMesh.o \
	$(OBJDIR)/Heed/EnTransfCS_BGM.o \
	$(OBJDIR)/Heed/EnTransfCS.o \
	$(OBJDIR)/Heed/HeedCluster.o \
	$(OBJDIR)/Heed/HeedCondElectron.o \
	$(OBJDIR)/Heed/HeedDeltaElectron.o \
	$(OBJDIR)/Heed/HeedDeltaElectronCS.o \
	$(OBJDIR)/Heed/HeedMatterDef.o \
	$(OBJDIR)/Heed/HeedParticle.o \
	$(OBJDIR)/Heed/HeedParticle_BGM.o \
	$(OBJDIR)/Heed/HeedPhoton.o \
	$(OBJDIR)/Heed/PairProd.o \
	$(OBJDIR)/Heed/PhotoAbsCS.o \
	$(OBJDIR)/Heed/PhotoAbsCSLib.o

HEEDOBJS += \
	$(OBJDIR)/Heed/HeedFieldMap.o \

# Compilation flags
CFLAGS = -std=c++11 -Wall -Wextra -pedantic -ansi -Wabi -Wno-long-long -Wshadow \
        `root-config --cflags` \
        -fpic -Os -c \
	-I$(INCDIR) -I$(HEEDDIR) \
	-DGARFIELD_HEED_INTERFACE -DUSE_SRANLUX \
	-DEXCLUDE_FUNCTIONS_WITH_HISTDEF -DINS_CRETURN 
# Debugging flags
#CFLAGS += -g

# Linking flags
LDFLAGS = `root-config --glibs` -lGeom -lgfortran -lm

all:	$(HEEDOBJS) 

clean:
	@echo Removing Heed object files...
	@$(RM) $(HEEDOBJS) 

$(OBJDIR)/Heed/FunNameStack.o: \
	$(HEEDDIR)/wcpplib/util/FunNameStack.cpp\
	$(HEEDDIR)/wcpplib/util/FunNameStack.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/definp.o: \
	$(HEEDDIR)/wcpplib/stream/definp.cpp\
	$(HEEDDIR)/wcpplib/stream/definp.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/findmark.o: \
	$(HEEDDIR)/wcpplib/stream/findmark.cpp\
	$(HEEDDIR)/wcpplib/stream/findmark.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/prstream.o: \
	$(HEEDDIR)/wcpplib/stream/prstream.cpp\
	$(HEEDDIR)/wcpplib/stream/prstream.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/AbsArr.o: \
	$(HEEDDIR)/wcpplib/safetl/AbsArr.cpp\
	$(HEEDDIR)/wcpplib/safetl/AbsArr.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/AbsPtr.o: \
	$(HEEDDIR)/wcpplib/safetl/AbsPtr.cpp\
	$(HEEDDIR)/wcpplib/safetl/AbsPtr.h 
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/chisran.o: \
	$(HEEDDIR)/wcpplib/random/chisran.cpp\
	$(HEEDDIR)/wcpplib/random/chisran.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/PointsRan.o: \
	$(HEEDDIR)/wcpplib/random/PointsRan.cpp\
	$(HEEDDIR)/wcpplib/random/PointsRan.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/pois.o: \
	$(HEEDDIR)/wcpplib/random/pois.cpp\
	$(HEEDDIR)/wcpplib/random/pois.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/rnorm.o: \
	$(HEEDDIR)/wcpplib/random/rnorm.cpp\
	$(HEEDDIR)/wcpplib/random/rnorm.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/eparticle.o: \
	$(HEEDDIR)/wcpplib/particle/eparticle.cpp\
	$(HEEDDIR)/wcpplib/particle/eparticle.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/particle_def.o: \
	$(HEEDDIR)/wcpplib/particle/particle_def.cpp\
	$(HEEDDIR)/wcpplib/particle/particle_def.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/AtomDef.o: \
	$(HEEDDIR)/wcpplib/matter/AtomDef.cpp\
	$(HEEDDIR)/wcpplib/matter/AtomDef.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/GasDef.o: \
	$(HEEDDIR)/wcpplib/matter/GasDef.cpp\
	$(HEEDDIR)/wcpplib/matter/GasDef.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/GasLib.o: \
	$(HEEDDIR)/wcpplib/matter/GasLib.cpp\
	$(HEEDDIR)/wcpplib/matter/GasLib.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/MatterDef.o: \
	$(HEEDDIR)/wcpplib/matter/MatterDef.cpp\
	$(HEEDDIR)/wcpplib/matter/MatterDef.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/MoleculeDef.o: \
	$(HEEDDIR)/wcpplib/matter/MoleculeDef.cpp\
	$(HEEDDIR)/wcpplib/matter/MoleculeDef.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/inverse.o: \
	$(HEEDDIR)/wcpplib/matrix/inverse.cpp\
	$(HEEDDIR)/wcpplib/matrix/inverse.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/multiply.o: \
	$(HEEDDIR)/wcpplib/matrix/multiply.cpp\
	$(HEEDDIR)/wcpplib/matrix/multiply.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/cubic.o: \
	$(HEEDDIR)/wcpplib/math/cubic.cpp\
	$(HEEDDIR)/wcpplib/math/cubic.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/DoubleAc.o: \
	$(HEEDDIR)/wcpplib/math/DoubleAc.cpp\
	$(HEEDDIR)/wcpplib/math/DoubleAc.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/kinem.o: \
	$(HEEDDIR)/wcpplib/math/kinem.cpp\
	$(HEEDDIR)/wcpplib/math/kinem.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/line.o: \
	$(HEEDDIR)/wcpplib/math/line.cpp\
	$(HEEDDIR)/wcpplib/math/line.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/linexi2.o: \
	$(HEEDDIR)/wcpplib/math/linexi2.cpp\
	$(HEEDDIR)/wcpplib/math/linexi2.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/lorgamma.o: \
	$(HEEDDIR)/wcpplib/math/lorgamma.cpp\
	$(HEEDDIR)/wcpplib/math/lorgamma.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/parabol.o: \
	$(HEEDDIR)/wcpplib/math/parabol.cpp\
	$(HEEDDIR)/wcpplib/math/parabol.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/PolLeg.o: \
	$(HEEDDIR)/wcpplib/math/PolLeg.cpp\
	$(HEEDDIR)/wcpplib/math/PolLeg.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/bethe_bloch.o: \
	$(HEEDDIR)/wcpplib/ioniz/bethe_bloch.cpp\
	$(HEEDDIR)/wcpplib/ioniz/bethe_bloch.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/e_cont_enloss.o: \
	$(HEEDDIR)/wcpplib/ioniz/e_cont_enloss.cpp\
	$(HEEDDIR)/wcpplib/ioniz/e_cont_enloss.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/box.o: \
	$(HEEDDIR)/wcpplib/geometry/box.cpp\
	$(HEEDDIR)/wcpplib/geometry/box.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/circumf.o: \
	$(HEEDDIR)/wcpplib/geometry/circumf.cpp\
	$(HEEDDIR)/wcpplib/geometry/circumf.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/gparticle.o: \
	$(HEEDDIR)/wcpplib/geometry/gparticle.cpp\
	$(HEEDDIR)/wcpplib/geometry/gparticle.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/mparticle.o: \
	$(HEEDDIR)/wcpplib/geometry/mparticle.cpp\
	$(HEEDDIR)/wcpplib/geometry/mparticle.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/plane.o: \
	$(HEEDDIR)/wcpplib/geometry/plane.cpp\
	$(HEEDDIR)/wcpplib/geometry/plane.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/polyline.o: \
	$(HEEDDIR)/wcpplib/geometry/polyline.cpp\
	$(HEEDDIR)/wcpplib/geometry/polyline.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/straight.o: \
	$(HEEDDIR)/wcpplib/geometry/straight.cpp\
	$(HEEDDIR)/wcpplib/geometry/straight.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/surface.o: \
	$(HEEDDIR)/wcpplib/geometry/surface.cpp\
	$(HEEDDIR)/wcpplib/geometry/surface.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/trajestep.o: \
	$(HEEDDIR)/wcpplib/geometry/trajestep.cpp\
	$(HEEDDIR)/wcpplib/geometry/trajestep.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/vec.o: \
	$(HEEDDIR)/wcpplib/geometry/vec.cpp\
	$(HEEDDIR)/wcpplib/geometry/vec.h \
	$(HEEDDIR)/wcpplib/geometry/vec.ic
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/volume.o: \
	$(HEEDDIR)/wcpplib/geometry/volume.cpp\
	$(HEEDDIR)/wcpplib/geometry/volume.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/BGMesh.o: \
	$(HEEDDIR)/heed++/code/BGMesh.cpp \
	$(HEEDDIR)/heed++/code/BGMesh.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/ElElasticScat.o: \
	$(HEEDDIR)/heed++/code/ElElasticScat.cpp \
	$(HEEDDIR)/heed++/code/ElElasticScat.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/EnergyMesh.o: \
	$(HEEDDIR)/heed++/code/EnergyMesh.cpp \
	$(HEEDDIR)/heed++/code/EnergyMesh.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/EnTransfCS_BGM.o: \
	$(HEEDDIR)/heed++/code/EnTransfCS_BGM.cpp \
	$(HEEDDIR)/heed++/code/EnTransfCS_BGM.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/EnTransfCS.o: \
	$(HEEDDIR)/heed++/code/EnTransfCS.cpp \
	$(HEEDDIR)/heed++/code/EnTransfCS.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedCluster.o: \
	$(HEEDDIR)/heed++/code/HeedCluster.cpp \
	$(HEEDDIR)/heed++/code/HeedCluster.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedCondElectron.o: \
	$(HEEDDIR)/heed++/code/HeedCondElectron.cpp \
	$(HEEDDIR)/heed++/code/HeedCondElectron.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedDeltaElectron.o: \
	$(HEEDDIR)/heed++/code/HeedDeltaElectron.cpp \
	$(HEEDDIR)/heed++/code/HeedDeltaElectron.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedDeltaElectronCS.o: \
	$(HEEDDIR)/heed++/code/HeedDeltaElectronCS.cpp \
	$(HEEDDIR)/heed++/code/HeedDeltaElectronCS.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedMatterDef.o: \
	$(HEEDDIR)/heed++/code/HeedMatterDef.cpp \
	$(HEEDDIR)/heed++/code/HeedMatterDef.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedParticle_BGM.o: \
	$(HEEDDIR)/heed++/code/HeedParticle_BGM.cpp \
	$(HEEDDIR)/heed++/code/HeedParticle_BGM.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedParticle.o: \
	$(HEEDDIR)/heed++/code/HeedParticle.cpp \
	$(HEEDDIR)/heed++/code/HeedParticle.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedPhoton.o: \
	$(HEEDDIR)/heed++/code/HeedPhoton.cpp \
	$(HEEDDIR)/heed++/code/HeedPhoton.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/PairProd.o: \
	$(HEEDDIR)/heed++/code/PairProd.cpp \
	$(HEEDDIR)/heed++/code/PairProd.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/PhotoAbsCS.o: \
	$(HEEDDIR)/heed++/code/PhotoAbsCS.cpp \
	$(HEEDDIR)/heed++/code/PhotoAbsCS.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/PhotoAbsCSLib.o: \
	$(HEEDDIR)/heed++/code/PhotoAbsCSLib.cpp \
	$(HEEDDIR)/heed++/code/PhotoAbsCSLib.h \
	$(HEEDDIR)/wcpplib/matter/GasLib.cpp\
	$(HEEDDIR)/wcpplib/matter/GasLib.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedFieldMap.o: \
	$(HEEDDIR)/HeedFieldMap.cpp \
	$(HEEDDIR)/HeedFieldMap.h \
	$(INCDIR)/Sensor.hh
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

