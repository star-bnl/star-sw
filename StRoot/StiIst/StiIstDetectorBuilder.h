#ifndef StiIstDetectorBuilder_h
#define StiIstDetectorBuilder_h

#include "Sti/StiDetectorBuilder.h"

class StIstDbMaker;


class StiIstDetectorBuilder : public StiDetectorBuilder
{
public:

   StiIstDetectorBuilder(bool active, const string &inputFile, bool buildIdealGeom=true);
   virtual void buildDetectors(StMaker &source);
   virtual void useVMCGeometry();

protected:

   StiMaterial  *mSiMaterial;
   StiMaterial  *mHybridMaterial;
   StIstDbMaker *mIstDb;
   bool          mBuildIdealGeom;

private:

   void buildInactiveVolumes();
};

#endif
