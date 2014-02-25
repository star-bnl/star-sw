#ifndef StiIstDetectorBuilder_h
#define StiIstDetectorBuilder_h

#include "Sti/StiDetectorBuilder.h"
#include "StThreeVector.hh"

class StIstDbMaker;


class StiIstDetectorBuilder : public StiDetectorBuilder
{
public:
   StiIstDetectorBuilder(bool active, const string &inputFile);
   virtual ~StiIstDetectorBuilder();
   virtual void buildDetectors(StMaker &source);
   virtual void useVMCGeometry();

   void    setSiMat(StiMaterial     *m) {_siMat = m;}
   void    setHybridMat(StiMaterial *m) {_hybridMat = m;}
   StiMaterial *getSiMat()    {return _siMat;}
   StiMaterial *getHybridMat() {return _hybridMat;}

protected:
   StiMaterial *_siMat;
   StiMaterial *_hybridMat;
   StiPlanarShape *_waferShape[1];
   StiPlanarShape *_hybridShape[1];
   StIstDbMaker   *mIstDb;
};

#endif
