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

   StiSstDetectorBuilder(bool active, const string &inputFile);
   virtual ~StiSstDetectorBuilder();
   virtual void buildDetectors(StMaker &source);
   virtual void useVMCGeometry();
   void         setSiMat(StiMaterial     *m) {_siMat = m;}
   void         setHybridMat(StiMaterial *m) {_hybridMat = m;}
   StiMaterial *getSiMat()    {return _siMat;}
   StiMaterial *getHybridMat() {return _hybridMat;}

protected:

   StSsdBarrel *mySsd;
   StiMaterial *_siMat;
   StiMaterial *_hybridMat;
   ssdWafersPosition_st *ssdWafersPosition(Int_t Id, St_ssdWafersPosition *wafers);

private:

   void buildInactiveVolumes();
};
#endif
