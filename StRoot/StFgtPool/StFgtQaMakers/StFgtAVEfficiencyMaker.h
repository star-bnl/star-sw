///
#ifndef _ST_FGT_AGV_EFF_MAKER_
#define _ST_FGT_AGV_EFF_MAKER_

#include "StMaker.h"
#include "StFgtQaMaker.h"
#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TFile.h>
//#include "StRoot/StEvent/StFgtCollection.h"


class StFgtAVEfficiencyMaker : public StFgtQaMaker {
 public:
  StFgtAVEfficiencyMaker(const Char_t* name="FgtAVEfficiencyMaker");

  virtual ~StFgtAVEfficiencyMaker();

   Int_t Init();
   Int_t Make();
   Int_t Finish();
   //   Bool_t checkPulse(StFgtHit* pClus);
   virtual const char *GetCVS() const
   {static const char cvs[]="Tag $Name:  $ $Id: StFgtAVEfficiencyMaker.h,v 1.1 2012/03/20 17:57:20 avossen Exp $ built "__DATE__" "__TIME__ ; return cvs;}
 protected:

   //   Double_t getRPhiRatio(StSPtrVecFgtHitConstIterator hitIterBegin, StSPtrVecFgtHitConstIterator hitIterEnd);
   //   Double_t getRPhiRatio();
   TH2D** radioPlotsEff;
   TH2D** radioPlotsNonEff;
   TH1D** rPhiRatioPlots;
   TH1D** rEff;
   TH1D** rNonEff;

   TFile* myRootFile;
   int runningEvtNr;
   int hitCounter;
   int hitCounterR;


   //THD2** 


 private:   
      ClassDef(StFgtAVEfficiencyMaker,1);


};

#endif

