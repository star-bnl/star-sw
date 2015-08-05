/* $Id: StIstFastSimMaker.h,v 1.23 2015/08/03 14:26:03 smirnovd Exp $ */

#ifndef StIstFastSimMaker_h
#define StIstFastSimMaker_h

#include "TRandom1.h"

#include "StMaker.h"

class StIstDb;
class THashList;


/**
 * Fast simulation maker for IST. IST GEANT hit is transformed to either ideal
 * or misaligned geometry of realistic detector, with smearing or pixelization.
 * The GEANT hit dE is directly propagated to IST hit in GeV.
 *
 * \author: Yaping Wang
 * \date August 2012
 */
class StIstFastSimMaker : public StMaker
{
public:

   StIstFastSimMaker(const Char_t *name = "istFastSim", bool useRandomSeed = false);
   Int_t Init();
   /// Retrieve GEANT hit information then transfer hit position to ideal or misaligned geometry of
   /// realistic IST detector
   Int_t Make();
   Int_t InitRun( Int_t runNo);
   /// Selects whether ideal or misalgined geometry is used mBuildIdealGeom kTRUE=ideal,
   /// kFALSE=misaligned
   void buildIdealGeom(Bool_t isIdealGeom) {mBuildIdealGeom = isIdealGeom;}

   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StIstFastSimMaker.h,v 1.23 2015/08/03 14:26:03 smirnovd Exp $ built " __DATE__ " " __TIME__  ;
      return cvs;
   }

private:

   THashList *mIstRot;
   StIstDb   *mIstDb;
   Bool_t     mBuildIdealGeom; ///< Switch between ideal and misaligned geometries. Default is true (ideal)
   TRandom1   mRandom;
   Double_t   mResXIst1;
   Double_t   mResZIst1;
   Bool_t     mSmear;          ///< Smear generated IST hit positions. Default is true

   /// Routine to smear hit by resolution with gaussian, mean zero and width res
   Double_t distortHit(const Double_t x, const Double_t res, const Double_t detLength);

   ClassDef(StIstFastSimMaker, 0)
};

#endif
