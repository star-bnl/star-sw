/* $Id: StIstFastSimMaker.h,v 1.11 2015/02/25 20:42:57 smirnovd Exp $ */

#ifndef STAR_StIstFastSimMaker
#define STAR_StIstFastSimMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif

class StRandom;
class StIstDb;
class THashList;

/**
 * Fast simulation maker for IST.
 *
 * \author: Yaping Wang
 * \date August 2012
 */
class StIstFastSimMaker : public StMaker
{
public:

   StIstFastSimMaker(const Char_t *name = "istFastSim");
   virtual ~StIstFastSimMaker();
   Int_t Init();
   //retrieve GEANT hit information, and transfer hit position to ideal/misaligned geometry of realistic IST detector
   Int_t Make();
   Int_t InitRun( Int_t runNo);
   //Selects whether ideal or misalgined geometry is used
   //mBuildIdealGeom kTRUE=ideal, kFALSE=misaligned
   void buildIdealGeom(Bool_t isIdealGeom) {mBuildIdealGeom = isIdealGeom;}
   virtual void  Clear(Option_t *option="");

   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StIstFastSimMaker.h,v 1.11 2015/02/25 20:42:57 smirnovd Exp $ built "__DATE__" "__TIME__ ;
      return cvs;
   }

protected:
   THashList *mIstRot;
   StIstDb *mIstDb;
   Bool_t mBuildIdealGeom;

   StRandom *mRandom;

   Double_t mResXIst1;
   Double_t mResZIst1;
   Bool_t mSmear; //to turn smearing on and off

private:

   /// Routine to smear hit by resolution with gaussian, mean zero and width res
   Double_t distortHit(const Double_t x, const Double_t res, const Double_t detLength) const;

   ClassDef(StIstFastSimMaker, 0)
};

#endif
