#ifndef StiIstDetectorBuilder_h
#define StiIstDetectorBuilder_h

#include "Sti/StiDetectorBuilder.h"

class StIstDbMaker;


class StiIstDetectorBuilder : public StiDetectorBuilder
{
public:

   StiIstDetectorBuilder(bool active, const string &inputFile);
   void buildDetectors(StMaker &source);
   void useVMCGeometry();

protected:

   StiMaterial  *mSiMaterial;
   StiMaterial  *mHybridMaterial;
   StIstDbMaker *mIstDb;

private:

   void buildInactiveVolumes();
};

#endif
