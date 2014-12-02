#ifndef StiSstDetectorBuilder1_H
#define StiSstDetectorBuilder1_H

#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StSsdDbMaker/StSstDbMaker.h"

class ssdWafersPosition_st;
class St_ssdWafersPosition;
class StiDetector;

class StiSstDetectorBuilder1 : public StiDetectorBuilder
{

public:

   StiSstDetectorBuilder1(bool active, bool buildIdealGeom=true);
   virtual ~StiSstDetectorBuilder1();
   virtual void buildDetectors(StMaker &source);
   virtual void useVMCGeometry();

protected:

   bool          mBuildIdealGeom;
   StSstDbMaker *mSstDb;

private:

   void buildInactiveVolumes();
   void segmentSFMOVolume(StiDetector* stiSFMO);
};
#endif
