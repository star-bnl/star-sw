#ifndef StiPxlDetectorBuilder_h
#define StiPxlDetectorBuilder_h

#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"

class StPxlDb;


class StiPxlDetectorBuilder : public StiDetectorBuilder
{
public:

   StiPxlDetectorBuilder(bool active, const string &inputFile);
   void buildDetectors(StMaker &source);
   void useVMCGeometry();

protected:

   StiMaterial *mSiMaterial;
   StiMaterial *mHybridMaterial;
   StPxlDb     *mPxlDb;
   Bool_t       mUseDbGeom;

private:

   void buildInactiveVolumes();
   void buildSimpleBox();
   void buildSimpleBox2();
   void buildSimplePlane();
   void buildSimpleTube();
   void buildSimpleTubeSector();

   enum EGeomDebug {kNoDebug, kSimpleBox, kSimpleBox2, kSimplePlane, kSimpleTube, kSimpleTubeSector};

   EGeomDebug mGeomDebug;
};

#endif
