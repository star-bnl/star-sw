#ifndef VecBosMcMaker_h
#define VecBosMcMaker_h

#include "TH2.h"
#include "TVector3.h"

#include "StMaker.h"


class StVecBosMaker;

/**
 * (Deprecated) A maker that fills histograms from MC containers.
 */
class VecBosMcMaker : public StMaker
{
public:

   VecBosMcMaker(const char *name = "2011pubMc");
   virtual       ~VecBosMcMaker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   void setHList(TObjArray *x) {HList = x;}

   void AttachWalgoMaker(StVecBosMaker *mk) { wMK = mk;}

   virtual Int_t InitRun  (int runumber) {return 0;}; // Overload empty StMaker::InitRun
   virtual Int_t FinishRun(int runumber) {return 0;}; // Overload empty StMaker::FinishRun

private:

   StVecBosMaker *wMK; // W-algo maker with all data

   // histograms
   TObjArray *HList;
   enum {mxHA = 128}; TH1 *hA[mxHA];

   void initHistos();
   void doWanalysis();
   void doWefficiency();
   bool doMCanalysis();

   TVector3 mWP;
   TVector3 mNeutrinoP;
   TVector3 mElectronP;
   TVector3 mVertex;

   ClassDef(VecBosMcMaker, 0)
};

#endif
