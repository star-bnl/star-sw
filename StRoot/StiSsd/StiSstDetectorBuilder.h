#ifndef StiSstDetectorBuilder_H
#define StiSstDetectorBuilder_H

#include <string>

#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StSsdDbMaker/StSstDbMaker.h"

class StiDetector;
class StiIsActiveFunctor;
class StiShape;
class StiPlacement;
class StiMaterial;


/**
 * This detector builder is responsible for constructing sensitive and inactive
 * Sti volumes describing the material of the SST detector. The corresponding
 * SST detector geometry is described in AgML files in StarVMC/Geometry/
 *
 * \author Dmitri Smirnov, BNL
 */
class StiSstDetectorBuilder : public StiDetectorBuilder
{

public:

   StiSstDetectorBuilder(bool active, bool buildIdealGeom=true);
   virtual void buildDetectors(StMaker &source);
   virtual void useVMCGeometry();

protected:

   virtual void buildInactiveVolumes();

   bool          mBuildIdealGeom;
   StSstDbMaker *mSstDb;
};
#endif
