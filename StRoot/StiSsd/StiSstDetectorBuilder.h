#ifndef StiSstDetectorBuilder_H
#define StiSstDetectorBuilder_H

#include "Sti/StiDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StThreeVector.hh"

class StSsdBarrel;
class ssdWafersPosition_st;
class St_ssdWafersPosition;
class StiSstDetectorBuilder : public StiDetectorBuilder
{

public:

   StiSstDetectorBuilder(bool active);
   virtual ~StiSstDetectorBuilder();
   virtual void buildDetectors(StMaker &source);
   virtual void useVMCGeometry();

protected:

   StSsdBarrel *mySsd;
   ssdWafersPosition_st *ssdWafersPosition(Int_t Id, St_ssdWafersPosition *wafers);

private:

   void buildInactiveVolumes();
};
#endif
