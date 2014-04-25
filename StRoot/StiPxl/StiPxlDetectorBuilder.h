#ifndef StiPxlDetectorBuilder_h
#define StiPxlDetectorBuilder_h

#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"

class StPxlDb;


class StiPxlDetectorBuilder : public StiDetectorBuilder
{
public:

   StiPxlDetectorBuilder(bool active, const string &inputFile);
   virtual void buildDetectors(StMaker &source);
   virtual void useVMCGeometry();

protected:

   StiMaterial *mSiMaterial;
   StiMaterial *mHybridMaterial;
   StPxlDb     *mPxlDb;
   Bool_t       mUseDbGeom;

private:

   void buildInactiveVolumes();
};

#endif
