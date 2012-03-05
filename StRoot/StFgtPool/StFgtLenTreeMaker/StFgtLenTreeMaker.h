///Cluster Plotter

#ifndef _ST_FGT_LEN_TREE_MAKER_
#define _ST_FGT_LEN_TREE_MAKER_

#include "StMaker.h"
#include "StRoot/StFgtPool/StFgtQaMakers/StFgtQaMaker.h"
#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TFile.h>
#include <TTree.h>

class StFgtLenTreeMaker : public StFgtQaMaker {
 public:
  StFgtLenTreeMaker(const Char_t* name="FgtLenTreeMaker");

  virtual ~StFgtLenTreeMaker();

   Int_t Init();
   Int_t Make();
   Int_t Finish();
   Int_t InitTree();
   //   Bool_t checkPulse(StFgtHit* pClus);
   TString fname;

 protected:
   ofstream* outTxtFile;
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
 private:   
   
   TFile* fFgt;
   TTree* tCl;
   
   Int_t iEvt;
   Int_t Ncl[6];
   Int_t cl_geoId[6][10];
   Int_t cl_quad[6][10];
   Float_t cl_z[6][10];
   Float_t cl_ez[6][10];
   Float_t cl_phi[6][10];
   Float_t cl_ephi[6][10];
   Float_t cl_r[6][10];
   Float_t cl_er[6][10];
   Float_t cl_charge[6][10];
   Float_t cl_echarge[6][10];
   Int_t cl_numStrips[6][10]; 
   Int_t cl_tStrip[6][10];
   Char_t cl_layer[6][10];
   Int_t cl_key[6][10];
   Int_t maxadc[6][10];
   Int_t seedadc[6][10];
 
   
   
   ClassDef(StFgtLenTreeMaker,1);


};

#endif
