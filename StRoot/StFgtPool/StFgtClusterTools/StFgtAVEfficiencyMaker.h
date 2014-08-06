///
#ifndef _ST_FGT_AGV_EFF_MAKER_
#define _ST_FGT_AGV_EFF_MAKER_

#include "StMaker.h"
//#include "StFgtQaMaker.h"
#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TFile.h>
#include "StFgtGeneralBase.h"
//#include "StRoot/StEvent/StFgtCollection.h"
class StFgtCollection;

class StFgtAVEfficiencyMaker : public StMaker {
 public:
  StFgtAVEfficiencyMaker(const Char_t* name="FgtAVEfficiencyMaker");

  virtual ~StFgtAVEfficiencyMaker();

   Int_t Init();
   Int_t Make();
   Int_t Finish();
   //   Bool_t checkPulse(StFgtHit* pClus);
   virtual const char *GetCVS() const
   {static const char cvs[]="Tag $Name:  $ $Id: StFgtAVEfficiencyMaker.h,v 1.2 2014/08/06 11:43:10 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
 protected:
   // for accessing the data
   StFgtCollection *mFgtCollectionPtr;

   // for knowing what & how to plot


   // threshold, in units of # sigma above average
   Float_t mPedThres;
   //   Double_t getRPhiRatio(StSPtrVecFgtHitConstIterator hitIterBegin, StSPtrVecFgtHitConstIterator hitIterEnd);
   //   Double_t getRPhiRatio();
   TH2D** radioPlotsEff;
   TH2D** radioPlotsNonEff;
   TH1D** rPhiRatioPlots;
   TH1D** rEff;
   TH1D** rNonEff;

   TH2D** chargeCorr;
   TH1D** h_clusterSizeR;
   TH1D** h_clusterSizePhi;
   TH1D** h_clusterChargeR;
   TH1D** h_clusterChargePhi;

   TH2D* hIp;
   TH1D* hIpZAtX0;
   TH1D* hIpZAtY0;

   TFile* myRootFile;
   int runningEvtNr;
   int hitCounter;
   int hitCounterR;


   //THD2** 


 private:   
      ClassDef(StFgtAVEfficiencyMaker,1);


};

#endif

