#ifndef St2011pubSpinMaker_h
#define St2011pubSpinMaker_h

#include "TString.h"

#include "StMaker.h"


class StVecBosMaker;


/**
 * Deprecated.
 */
class St2011pubSpinMaker : public StMaker
{
private:

   float par_QPTlow, par_QPThighET0, par_QPThighET1, par_QPThighA , par_QPThighB; // cuts to drop questionable reco charge charges
   float par_leptonEta1, par_leptonEta2; // narrow the range
   int   par_useNoEEMC;

   StVecBosMaker *wMK; // W-algo maker with all data
   TString core;      // name attached to all histos
   TString coreTitle; // eta bin name added title of key histos

   // histograms
   TObjArray *HList;
   enum {mxHA = 32};
   TH1 *hA[mxHA];

   void initHistos();
   void bXingSort();

public:

   St2011pubSpinMaker(const char *name = "2011pubSpin", const char *etaName = "Eta7");
   virtual       ~St2011pubSpinMaker() {};
   virtual Int_t  Init();
   virtual Int_t  InitRun  (int runumber);
   virtual Int_t  Make();
   void setHList(TObjArray *x) {HList = x;}
   void setEta(float x, float y) { par_leptonEta1 = x; par_leptonEta2 = y;};
   void setQPT(float x) {  par_QPTlow = x;}
   void setNoEEMC() {par_useNoEEMC = 1;}

   void attachWalgoMaker(StVecBosMaker *mk) { wMK = mk;}
   virtual Int_t FinishRun(int runumber);

   ClassDef(St2011pubSpinMaker, 0)
};

#endif
