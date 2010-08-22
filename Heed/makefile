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
	$(OBJDIR)/Heed/eiparticle.o \
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
	$(OBJDIR)/Heed/line.o \
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
	$(OBJDIR)/Heed/volume_int.o \
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
	$(OBJDIR)/Heed/HeedPhoton.o \
	$(OBJDIR)/Heed/PairProd.o \
	$(OBJDIR)/Heed/ParticleBank.o \
	$(OBJDIR)/Heed/PhotoAbsCS.o \
	$(OBJDIR)/Heed/PhotoAbsCSLib.o

# Compilation flags
CFLAGS = -Wall -Wextra -pedantic -Wabi -Wno-long-long \
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
	$(HEEDDIR)/wcpplib/util/FunNameStack.c \
	$(HEEDDIR)/wcpplib/util/FunNameStack.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/definp.o: \
	$(HEEDDIR)/wcpplib/stream/definp.c \
	$(HEEDDIR)/wcpplib/stream/definp.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/findmark.o: \
	$(HEEDDIR)/wcpplib/stream/findmark.c \
	$(HEEDDIR)/wcpplib/stream/findmark.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/prstream.o: \
	$(HEEDDIR)/wcpplib/stream/prstream.c \
	$(HEEDDIR)/wcpplib/stream/prstream.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/AbsArr.o: \
	$(HEEDDIR)/wcpplib/safetl/AbsArr.c \
	$(HEEDDIR)/wcpplib/safetl/AbsArr.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/AbsPtr.o: \
	$(HEEDDIR)/wcpplib/safetl/AbsPtr.c \
	$(HEEDDIR)/wcpplib/safetl/AbsPtr.h 
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/chisran.o: \
	$(HEEDDIR)/wcpplib/random/chisran.c \
	$(HEEDDIR)/wcpplib/random/chisran.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/PointsRan.o: \
	$(HEEDDIR)/wcpplib/random/PointsRan.c \
	$(HEEDDIR)/wcpplib/random/PointsRan.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/pois.o: \
	$(HEEDDIR)/wcpplib/random/pois.c \
	$(HEEDDIR)/wcpplib/random/pois.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/rnorm.o: \
	$(HEEDDIR)/wcpplib/random/rnorm.c \
	$(HEEDDIR)/wcpplib/random/rnorm.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/eiparticle.o: \
	$(HEEDDIR)/wcpplib/particle/eiparticle.c \
	$(HEEDDIR)/wcpplib/particle/eiparticle.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/eparticle.o: \
	$(HEEDDIR)/wcpplib/particle/eparticle.c \
	$(HEEDDIR)/wcpplib/particle/eparticle.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/particle_def.o: \
	$(HEEDDIR)/wcpplib/particle/particle_def.c \
	$(HEEDDIR)/wcpplib/particle/particle_def.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/AtomDef.o: \
	$(HEEDDIR)/wcpplib/matter/AtomDef.c \
	$(HEEDDIR)/wcpplib/matter/AtomDef.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/GasDef.o: \
	$(HEEDDIR)/wcpplib/matter/GasDef.c \
	$(HEEDDIR)/wcpplib/matter/GasDef.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/GasLib.o: \
	$(HEEDDIR)/wcpplib/matter/GasLib.c \
	$(HEEDDIR)/wcpplib/matter/GasLib.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/MatterDef.o: \
	$(HEEDDIR)/wcpplib/matter/MatterDef.c \
	$(HEEDDIR)/wcpplib/matter/MatterDef.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/MoleculeDef.o: \
	$(HEEDDIR)/wcpplib/matter/MoleculeDef.c \
	$(HEEDDIR)/wcpplib/matter/MoleculeDef.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/inverse.o: \
	$(HEEDDIR)/wcpplib/matrix/inverse.c \
	$(HEEDDIR)/wcpplib/matrix/inverse.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/multiply.o: \
	$(HEEDDIR)/wcpplib/matrix/multiply.c \
	$(HEEDDIR)/wcpplib/matrix/multiply.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/cubic.o: \
	$(HEEDDIR)/wcpplib/math/cubic.c \
	$(HEEDDIR)/wcpplib/math/cubic.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/DoubleAc.o: \
	$(HEEDDIR)/wcpplib/math/DoubleAc.c \
	$(HEEDDIR)/wcpplib/math/DoubleAc.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/kinem.o: \
	$(HEEDDIR)/wcpplib/math/kinem.c \
	$(HEEDDIR)/wcpplib/math/kinem.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/line.o: \
	$(HEEDDIR)/wcpplib/math/line.c \
	$(HEEDDIR)/wcpplib/math/line.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/linexi2.o: \
	$(HEEDDIR)/wcpplib/math/linexi2.c \
	$(HEEDDIR)/wcpplib/math/linexi2.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/lorgamma.o: \
	$(HEEDDIR)/wcpplib/math/lorgamma.c \
	$(HEEDDIR)/wcpplib/math/lorgamma.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/parabol.o: \
	$(HEEDDIR)/wcpplib/math/parabol.c \
	$(HEEDDIR)/wcpplib/math/parabol.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/PolLeg.o: \
	$(HEEDDIR)/wcpplib/math/PolLeg.c \
	$(HEEDDIR)/wcpplib/math/PolLeg.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/bethe_bloch.o: \
	$(HEEDDIR)/wcpplib/ioniz/bethe_bloch.c \
	$(HEEDDIR)/wcpplib/ioniz/bethe_bloch.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/e_cont_enloss.o: \
	$(HEEDDIR)/wcpplib/ioniz/e_cont_enloss.c \
	$(HEEDDIR)/wcpplib/ioniz/e_cont_enloss.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/box.o: \
	$(HEEDDIR)/wcpplib/geometry/box.c \
	$(HEEDDIR)/wcpplib/geometry/box.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/circumf.o: \
	$(HEEDDIR)/wcpplib/geometry/circumf.c \
	$(HEEDDIR)/wcpplib/geometry/circumf.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/gparticle.o: \
	$(HEEDDIR)/wcpplib/geometry/gparticle.c \
	$(HEEDDIR)/wcpplib/geometry/gparticle.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/mparticle.o: \
	$(HEEDDIR)/wcpplib/geometry/mparticle.c \
	$(HEEDDIR)/wcpplib/geometry/mparticle.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/plane.o: \
	$(HEEDDIR)/wcpplib/geometry/plane.c \
	$(HEEDDIR)/wcpplib/geometry/plane.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/polyline.o: \
	$(HEEDDIR)/wcpplib/geometry/polyline.c \
	$(HEEDDIR)/wcpplib/geometry/polyline.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/straight.o: \
	$(HEEDDIR)/wcpplib/geometry/straight.c \
	$(HEEDDIR)/wcpplib/geometry/straight.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/surface.o: \
	$(HEEDDIR)/wcpplib/geometry/surface.c \
	$(HEEDDIR)/wcpplib/geometry/surface.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/trajestep.o: \
	$(HEEDDIR)/wcpplib/geometry/trajestep.c \
	$(HEEDDIR)/wcpplib/geometry/trajestep.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/vec.o: \
	$(HEEDDIR)/wcpplib/geometry/vec.c \
	$(HEEDDIR)/wcpplib/geometry/vec.h \
	$(HEEDDIR)/wcpplib/geometry/vec.ic
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/volume_int.o: \
	$(HEEDDIR)/wcpplib/geometry/volume_int.c \
	$(HEEDDIR)/wcpplib/geometry/volume_int.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/volume.o: \
	$(HEEDDIR)/wcpplib/geometry/volume.c \
	$(HEEDDIR)/wcpplib/geometry/volume.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/BGMesh.o: \
	$(HEEDDIR)/heed++/code/BGMesh.c \
	$(HEEDDIR)/heed++/code/BGMesh.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/ElElasticScat.o: \
	$(HEEDDIR)/heed++/code/ElElasticScat.c \
	$(HEEDDIR)/heed++/code/ElElasticScat.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/EnergyMesh.o: \
	$(HEEDDIR)/heed++/code/EnergyMesh.c \
	$(HEEDDIR)/heed++/code/EnergyMesh.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/EnTransfCS_BGM.o: \
	$(HEEDDIR)/heed++/code/EnTransfCS_BGM.c \
	$(HEEDDIR)/heed++/code/EnTransfCS_BGM.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/EnTransfCS.o: \
	$(HEEDDIR)/heed++/code/EnTransfCS.c \
	$(HEEDDIR)/heed++/code/EnTransfCS.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedCluster.o: \
	$(HEEDDIR)/heed++/code/HeedCluster.c \
	$(HEEDDIR)/heed++/code/HeedCluster.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedCondElectron.o: \
	$(HEEDDIR)/heed++/code/HeedCondElectron.c \
	$(HEEDDIR)/heed++/code/HeedCondElectron.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedDeltaElectron.o: \
	$(HEEDDIR)/heed++/code/HeedDeltaElectron.c \
	$(HEEDDIR)/heed++/code/HeedDeltaElectron.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedDeltaElectronCS.o: \
	$(HEEDDIR)/heed++/code/HeedDeltaElectronCS.c \
	$(HEEDDIR)/heed++/code/HeedDeltaElectronCS.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedMatterDef.o: \
	$(HEEDDIR)/heed++/code/HeedMatterDef.c \
	$(HEEDDIR)/heed++/code/HeedMatterDef.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedParticle_BGM.o: \
	$(HEEDDIR)/heed++/code/HeedParticle_BGM.c \
	$(HEEDDIR)/heed++/code/HeedParticle_BGM.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedParticle.o: \
	$(HEEDDIR)/heed++/code/HeedParticle.c \
	$(HEEDDIR)/heed++/code/HeedParticle.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/HeedPhoton.o: \
	$(HEEDDIR)/heed++/code/HeedPhoton.c \
	$(HEEDDIR)/heed++/code/HeedPhoton.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/PairProd.o: \
	$(HEEDDIR)/heed++/code/PairProd.c \
	$(HEEDDIR)/heed++/code/PairProd.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/ParticleBank.o: \
	$(HEEDDIR)/heed++/code/ParticleBank.c \
	$(HEEDDIR)/heed++/code/ParticleBank.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/PhotoAbsCS.o: \
	$(HEEDDIR)/heed++/code/PhotoAbsCS.c \
	$(HEEDDIR)/heed++/code/PhotoAbsCS.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

$(OBJDIR)/Heed/PhotoAbsCSLib.o: \
	$(HEEDDIR)/heed++/code/PhotoAbsCSLib.c \
	$(HEEDDIR)/heed++/code/PhotoAbsCSLib.h \
	$(HEEDDIR)/wcpplib/matter/GasLib.c \
	$(HEEDDIR)/wcpplib/matter/GasLib.h
	@echo $@
	@$(CXX) $(CFLAGS) $< -o $@

