///Cluster Plotter

#ifndef _ST_FGT_CLUSTER_PLOT_MAKER_
#define _ST_FGT_CLUSTER_PLOT_MAKER_

#include "StMaker.h"
#include "StFgtQaMaker.h"
#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TFile.h>

class StFgtClusterPlotter : public StFgtQaMaker {
 public:
  StFgtClusterPlotter(const Char_t* name="FgtClusterPlotter");

  virtual ~StFgtClusterPlotter();

   Int_t Init();
   Int_t Make();
   Int_t Finish();
   //   Bool_t checkPulse(StFgtHit* pClus);

 protected:
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
   TH2D** corrPlots;
   int runningEvtNr;


   //THD2** 


 private:   
   ClassDef(StFgtClusterPlotter,1);


};

#endif
