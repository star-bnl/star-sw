 /*
   \class StSstFastSimMaker

   \brief Class to simulate Ssd from GEANT.

   Currently, we read in the SSD hits from the GEANT hit table, smear the hit on the wafer, and fill the SSD Hit Collection with the smeared points.
   This class conforms to the STAR StMaker standards.
*/

#ifndef STAR_StSstFastSimMaker
#define STAR_StSstFastSimMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StSstUtil/StSstBarrel.hh"
#include "StSstUtil/StSstWafer.hh"
class StEvent;
class StMcEvent;
class StRandom;

class StSstBarrel;
class sstDimensions_st;
class St_g2t_ssd_hit;
class St_sls_strip;
class sstConfiguration_st;
class StSstHit;
class StSstHitCollection;
class StSstFastSimMaker : public StMaker {
 private :
  sstDimensions_st     *mDimensions;//!
 public:
  StSstFastSimMaker(const char *name="SstFastSim");   
  virtual       ~StSstFastSimMaker();
  virtual Int_t Make();
  virtual Int_t Init();
  virtual Int_t InitRun(int);
  virtual void Clear(Option_t *option="");
  virtual Int_t Finish();
  virtual Bool_t accept(StEvent* event);

  Float_t   distortHit(float x, double res);
  void      setSstParameters(sstDimensions_st *geom_par);  
  Int_t     idWaferToWaferNumb(int idWafer); //  idwafer = layer*1000+waf*100+ladder => waferNumb = mNWaferPerLadder*(ladder-1) + waf - 1
  Int_t     idWaferToLadderNumb(int idWafer);//  idwafer => ladder-1
  Int_t     waferNumbToIdWafer(int waferNumb);// waferNumb = mNWaferPerLadder*(ladder-1) + waf - 1 => idwafer
  Int_t     idWaferToWafer(int idWafer) {return (idWafer-7000)/100-1;}
  Int_t     IsOnWafer(float x, float y);
  Int_t     RemoveTriangle(float x, float y);
 
 private :
  Int_t    mSstLayer;
  Int_t    mNLadder;
  Int_t    mNWaferPerLadder;
  Int_t    mNStripPerSide;
  Int_t    mActiveLadders[20];
  Float_t  mDetectorLargeEdge;
  Float_t  mDetectorSmallEdge;
  Float_t  mStripPitch;
  Float_t  mTheta;
  
 protected:
  StRandom* mRandom;
  double    mResXSst;
  double    mResZSst;
  StEvent   *mEvent;

 virtual const char *GetCVS() const
  {
    static const char cvs[]="Tag $Name:  $ $Id: StSstFastSimMaker.h,v 1.3 2015/08/04 18:47:38 bouchet Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  ClassDef(StSstFastSimMaker,0)   //StAF chain virtual base class for Makers
};
#endif
