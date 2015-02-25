/***************************************************************************
*
* $Id: StIstFastSimMaker.h,v 1.1 2015/02/25 20:03:38 smirnovd Exp $
*
* Author: Yaping Wang, August 2012
****************************************************************************
* Description: 
* Fast simulation maker for IST.
****************************************************************************
*
* $Log: StIstFastSimMaker.h,v $
* Revision 1.1  2015/02/25 20:03:38  smirnovd
* New simulation maker for IST
*
* Revision 1.4  2014/08/05 03:28:42  ypwang
* buildIdealGeom() added to switch between ideal VMC geometry or DB geometry, Db geometry was built by default
*
* Revision 1.3  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstFastSimMaker.h,v 1.0
* Revision 1.0 2013/11/04 16:25:30 Yaping
* Initial version
****************************************************************************/

#ifndef STAR_StIstFastSimMaker
#define STAR_StIstFastSimMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include <vector>

class StEvent;
class StMcEvent;
class StRandom;
class StMcIstHitCollection;
class StIstHitCollection;
class St_g2t_ist_hit;

class StIstDb;
class THashList;

class TString;

class StIstFastSimMaker : public StMaker {
 public:

  StIstFastSimMaker(const char *name="istFastSim");
  Int_t Make();
  Int_t Finish();
  Int_t Init();
  Int_t InitRun( int);
  void buildIdealGeom(Bool_t isIdealGeom) {mBuildIdealGeom = isIdealGeom;} 
 
  //Routine to smear hit by resolution with gaussian, mean zero and width res
  Double_t distortHit(double x, double res, double detLength);

  virtual const char *GetCVS() const
  {
    static const char cvs[]="Tag $Name:  $ $Id: StIstFastSimMaker.h,v 1.1 2015/02/25 20:03:38 smirnovd Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

 protected:
  THashList *istRot;
  StIstDb *mIstDb;
  Bool_t mBuildIdealGeom;

  StRandom* myRandom;

  Double_t resXIst1;
  Double_t resZIst1;
  Int_t mSmear; //to turn smearing on and off

  private:

  ClassDef(StIstFastSimMaker,1)   //StAF chain virtual base class for Makers
};
#endif
