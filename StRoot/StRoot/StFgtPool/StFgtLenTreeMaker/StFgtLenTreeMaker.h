///Cluster Plotter

#ifndef _ST_FGT_LEN_TREE_MAKER_
#define _ST_FGT_LEN_TREE_MAKER_

#include "StMaker.h"
#include "StRoot/StFgtPool/StFgtQaMakers/StFgtQaMaker.h"
#include <TH2D.h>
#include <TH1F.h>
#include <TF1.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TFile.h>
#include <TTree.h>

class StFgtCollection;
class StFgtHitCollection;
class StFgtHit;

class StFgtLenTrack {
 public:
  Float_t phi;
  Float_t slope;
  Float_t vtx;
  Float_t chi2;
  Int_t ncluster;
  StFgtHit* clArray[6];
};

class StFgtLenTreeMaker : public StFgtQaMaker {
 public:
  StFgtLenTreeMaker(const Char_t* name="FgtLenTreeMaker");

  virtual ~StFgtLenTreeMaker();

   Int_t Init();
   Int_t Make();
   Int_t Finish();
   Int_t InitTree();
   void FitFunc();
   void InitFX();
   TString fname;
   Int_t fitThresh;
   Int_t Ntimebin;
   StFgtLenTrack getLenTrack( int iTrk );

 protected:
   int runningEvtNr;
   Int_t Ntrk;
   
 private:   

   struct MyFunc;
   /*
   struct StFgtLenTrack {
     Float_t phi;
     Float_t slope;
     Float_t vtx;
     Float_t chi2;
     Int_t ncluster;
     StFgtHit* clArray[6];
   };
   */
   StFgtLenTrack trkArray[4];

   
   TFile* fFgt;

   TTree* tCl;
   
   Int_t iEvt;
   Int_t Ncl[6];
   Int_t cl_geoId[6][20];
   Int_t cl_seedType[6][20];
   Int_t cl_quad[6][20];
   Float_t cl_z[6][20];
   Float_t cl_ez[6][20];
   Float_t cl_phi[6][20];
   Float_t cl_ephi[6][20];
   Float_t cl_r[6][20];
   Float_t cl_er[6][20];
   Float_t cl_charge[6][20];
   Float_t cl_echarge[6][20];
   Int_t cl_numStrips[6][20]; 
   Int_t cl_tQuad[6][20];
   Int_t cl_tStrip[6][20];
   Char_t cl_layer[6][20];
   Int_t cl_key[6][20];
   Int_t maxadc[6][20];
   Int_t seedadc[6][20];
   Float_t tr_phi[4];
   Float_t tr_slope[4];
   Float_t tr_vtx[4];
   Float_t tr_chi2[4];
   Int_t tr_ncluster[4];
   Int_t tr_iCl[4][6];


   TTree* tFgt;
  	
   Int_t rdo;
   Int_t arm;
   Int_t apv;
   Int_t chn;
   Short_t disk;
   Short_t quad;
   Short_t strip;
   Short_t stat;
   Double_t ordinate;
   Double_t lowerSpan;
   Double_t upperSpan;
   Char_t layer;
   Double_t ped;
   Double_t pedSig;
   Int_t adc[7];
   Int_t adcmax;
   Int_t mmax;
   Int_t mmin;
   Float_t chi2;
   Float_t fmax;
   Float_t norm;
   Float_t tau;
   Float_t t0;   
   Float_t beta;
   Float_t offset;
   Int_t errCode;
   
   TF1* FX;
   TF1* fs;
   
   StFgtHit* cl_pointers[6][20];

   TH1F* htrk;
   TF1* f0;
   TH1F* hh;

   ClassDef(StFgtLenTreeMaker,1);
};

inline StFgtLenTrack StFgtLenTreeMaker::getLenTrack( int iTrk ) {
  return trkArray[iTrk];
};

#endif
