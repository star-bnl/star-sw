#ifndef EFFICIENCY_H
#define EFFICIENCY_H

#include <TObject.h>
#include <TString.h>
#include <TPostScript.h>
#include <TH1F.h>
#include <TH2F.h>
#include <TFile.h>
#include <TTree.h>

class MyEvent;

class AnaCuts;

class Efficiency : public TObject{
 protected:
  TFile *mFile;
  TFile *mFileOut;
  TTree *myEventTree;
  AnaCuts *cuts;
  MyEvent *ev; //!
  TPostScript *pseff;

  Bool_t isDAU;
  Bool_t isPP05;

  Bool_t NEUTRONS;
  Bool_t ANTINEUTRONS;
  Bool_t PIONS;
  Bool_t ETAS;
  Bool_t CHARGEDPIONS;
  Bool_t PHOTONS;
  Bool_t KZEROLONG;

  Bool_t isMC;  
  Bool_t isPythia;
  Bool_t USEWEIGHT;
  Bool_t USEPYTHIAWEIGHT;
  Bool_t USEBBCSPREAD;
  const char *mFlag;

  Float_t nNeutrons;

  TString htitle;
  TString dirout;

  //------
  TH2F *h_accMB;
  TH2F *h_genMB;

  TH2F *h_HT1adc_id;
  TH2F *h_HT2adc_id;

  TH1F *h_convGen;
  TH1F *h_convConv;
  TH1F *h_convConvSSD;
  TH1F *h_convConvSVT;
  TH1F *h_convConvIFC;

  TH1F *h_convConvNotCtb;
  TH1F *h_stopRadius;

  TH1F *h_mcdist;
  TH2F *h_mcdist2D;
  TH1F *h_dist;
  TH2F *h_dist2D;

  TH1F *h_nbarDet;
  TH1F *h_nbarIn;

  TH1F *h_splitClusAll;
  TH1F *h_splitClus;

  TH1F *h_inputMB;
  TH1F *h_inputHT1;
  TH1F *h_inputHT2;

  TH1F *h_inputDaughtersMB;
  TH1F *h_inputDaughtersHT1;
  TH1F *h_inputDaughtersHT2;

  TH1F *h_recoMB;
  TH1F *h_recoHT1;
  TH1F *h_recoHT2;

  TH1F *h_recoDaughtersMB;
  TH1F *h_recoDaughtersHT1;
  TH1F *h_recoDaughtersHT2;

  TH2F *h_minvMB;
  TH2F *h_minvHT1;
  TH2F *h_minvHT2;

  TH1F *h_effMB;
  TH1F *h_effHT1;
  TH1F *h_effHT2;
  
  TH1F *h_effDaughtersMB;
  TH1F *h_effDaughtersHT1;
  TH1F *h_effDaughtersHT2;

  TH2F *h_matrixMB;
  TH2F *h_etaphi;

  TH1F *h_pythiaPions;
  TH1F *h_pythiaPhotons;
  TH1F *h_pythiaPartonPt;

  TH2F *h_clusterWidth;
  TH2F *h_energyRatio;
  TH2F *h_towclusRatio;

  TH1F *h_vzMB;

  TH2F *h_asymmMB;
  TH2F *h_asymmHT1;
  TH2F *h_asymmHT2;

  TH1F *h_pionsVsEtaMB;

  TH2F *h_smdeta1;
  TH2F *h_smdphi1;
  TH2F *h_smdeta2;
  TH2F *h_smdphi2;

  TH1F *h_energyeta;
  TH1F *h_energyphi;


 public:

  Efficiency(const char*,const char*,const char*);
  ~Efficiency();

  Int_t init();
  Int_t make(Int_t i=0);
  Int_t finish();

  Float_t getWeightPions(Float_t);
  Float_t getWeightEtas(Float_t);
  Float_t getWeightAntiNeutrons(Float_t);
  Float_t getWeightNeutrons(Float_t);
  Float_t getWeightPhotons(Float_t);
  Float_t getWeightVertex(Float_t);

  void setUseWeight(Bool_t val){USEWEIGHT=val;}
  void setUsePythiaWeight(Bool_t val){USEPYTHIAWEIGHT=val;}
  void setMC(Bool_t val) {isMC=val;}
  void setPythia(Bool_t val) {isPythia=val;}
  void setUseBbcSpread(Bool_t val){USEBBCSPREAD=val;}
  ClassDef(Efficiency,0)
};

#endif
