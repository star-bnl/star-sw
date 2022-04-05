///
#ifndef _ST_FGT_GEN_CLUS_PLOTTER_
#define _ST_FGT_GEN_CLUS_PLOTTER_

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

class StFgtGenPlotter : public StFgtGeneralBase {
 public:
  StFgtGenPlotter(const Char_t* name="FgtGenPlotter");

  virtual ~StFgtGenPlotter();

   Int_t Init();
   Int_t Make();
   Int_t Finish();
   //   Bool_t checkPulse(StFgtHit* pClus);
   virtual const char *GetCVS() const
   {static const char cvs[]="Tag $Name:  $ $Id: StFgtGenPlotter.h,v 1.2 2014/08/06 11:43:10 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
 protected:
   // for accessing the data
   StFgtCollection *mFgtCollectionPtr;

   ofstream* outTxtFile;
   ofstream* outTxtFileR;
   ofstream* outTxtFileP;

   TFile* myRootFile;
   TH1* hClusterCharge;
   TH2D** hCChargePosSpacePhi;
   TH2D** hCChargePosSpaceR;
   TH2D** hClusSizePhi;
   TH2D** hClusSizeR;
   TH2D** hCChargeElecSpace;
   TH2D** hClusSizeElecSpace;
   TH2D** radioPlots;
   TH2D** radioRatio;
   TH2D** radioChargeR;
   TH2D** radioChargePhi;
   TH2D** radioClusSizeR;
   TH2D** radioClusSizePhi;



   TH2D** corrPlots;
   int runningEvtNr;

   TH2D** radioPlotsEff;
   TH2D** radioPlotsNonEff;
   TH1D** rPhiRatioPlots;
   TH1D** rEff;
   TH1D** rNonEff;

   TH1I** multPerDiscR;
   TH1I** seedsPerDiscR;
   TH1I** multPerDiscP;
   TH1I** seedsPerDiscP;


   TH2D** chargeCorr;
   TH1D** h_clusterSizeR;
   TH1D** h_clusterSizePhi;
   TH1D** h_clusterChargeR;
   TH1D** h_clusterChargePhi;


   TH2D* hIp;
   TH1D* hIpZAtX0;
   TH1D* hIpZAtY0;

   int hitCounter;
   int hitCounterR;


   //THD2** 
 private:   
      ClassDef(StFgtGenPlotter,1);


};

#endif

