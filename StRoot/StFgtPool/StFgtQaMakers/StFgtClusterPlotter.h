///Cluster Plotter

#ifndef _ST_FGT_CLUSTER_PLOT_MAKER_
#define _ST_FGT_CLUSTER_PLOT_MAKER_

#include "StMaker.h"
#include "StFgtQaMaker.h"
#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>


class StFgtClusterPlotMaker : public StFgtQaMaker {
 public:
  StFgtClusterPlotMaker(const Char_t* name="FgtClusterPlotMaker");

  virtual ~StFgtClusterPlotMaker();

   Int_t Init();
   Int_t Make();
   Int_t Finish();

 protected:
   TH1* hClusterCharge;
   TH2D** hCChargePosSpacePhi;
   TH2D** hCChargePosSpaceR;
   TH2D** hClusSizePhi;
   TH2D** hClusSizeR;
   TH2D** hCChargeElecSpace;
   TH2D** hClusSizeElecSpace;


};

#endif
