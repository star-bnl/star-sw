#ifndef StiSstDetectorBuilder_H
#define StiSstDetectorBuilder_H

#include <string>

#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StSsdDbMaker/StSstDbMaker.h"

class ssdWafersPosition_st;
class St_ssdWafersPosition;
class StiDetector;
class StiIsActiveFunctor;
class StiShape;
class StiPlacement;
class StiMaterial;


class StiSstDetectorBuilder : public StiDetectorBuilder
{

public:

   StiSstDetectorBuilder(bool active, bool buildIdealGeom=true);
   virtual ~StiSstDetectorBuilder();
   virtual void buildDetectors(StMaker &source);
   virtual void useVMCGeometry();

protected:

   virtual void buildInactiveVolumes();

   bool          mBuildIdealGeom;
   StSstDbMaker *mSstDb;

private:

   void setDetectorProperties(StiDetector* detector, std::string name, StiIsActiveFunctor* activeFunctor, StiShape* shape, StiPlacement* placement, StiMaterial* gas, StiMaterial* material);
};
#endif
