#ifndef StVecBosSpinMaker_h
#define StVecBosSpinMaker_h

#include "TString.h"

#include "StMaker.h"


class StVecBosMaker;


class StVecBosSpinMaker : public StMaker
{
private:

   float par_QPTlow, par_QPThighET0, par_QPThighET1, par_QPThighA , par_QPThighB; // cuts to drop questionable reco charge charges
   float par_leptonEta1, par_leptonEta2; // narrow the range
   int   par_useNoEEMC;

   StVecBosMaker *wMK; // W-algo maker with all data
   TString core;       // name attached to all histos
   TString coreTitle;  // eta bin name added title of key histos

   // histograms
   TObjArray *HList;
   enum {mxHA = 32};
   TH1 *hA[mxHA];

   void InitHistos();
   void bXingSort();

public:

   StVecBosSpinMaker(const char *name = "2011pubSpin", const char *etaName = "Eta7");
   virtual ~StVecBosSpinMaker() {};
   virtual Int_t Init();
   virtual Int_t InitRun  (int runumber);
   virtual Int_t Make();
   virtual Int_t FinishRun(int runumber);
   virtual const char *GetCVS() const
   {
      static const char cvs[] = "Tag $Name:  $ $Id: StVecBosSpinMaker.h,v 1.1 2013/01/10 21:46:34 smirnovd Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
   }

   void SetHList(TObjArray *x)         { HList = x; }
   void setEta(float eta1, float eta2) { par_leptonEta1 = eta1; par_leptonEta2 = eta2; }
   void setQPT(float lowQPt)           { par_QPTlow = lowQPt; }
   void setNoEEMC()                    { par_useNoEEMC = 1; }

   void AttachWalgoMaker(StVecBosMaker *mk) { wMK = mk;}

   ClassDef(StVecBosSpinMaker, 0)
};

#endif
