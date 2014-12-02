#ifndef StiSstDetectorBuilder_H
#define StiSstDetectorBuilder_H

#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StSsdDbMaker/StSstDbMaker.h"

class ssdWafersPosition_st;
class St_ssdWafersPosition;
class StiDetector;

class StiSstDetectorBuilder : public StiDetectorBuilder
{

public:

   StiSstDetectorBuilder(bool active, bool buildIdealGeom=true);
   virtual ~StiSstDetectorBuilder();
   virtual void buildDetectors(StMaker &source);
   virtual void useVMCGeometry();

protected:

   virtual void buildInactiveVolumes();
   virtual void segmentSFMOVolume(StiDetector* stiSFMO);

   bool          mBuildIdealGeom;
   StSstDbMaker *mSstDb;
};
#endif
