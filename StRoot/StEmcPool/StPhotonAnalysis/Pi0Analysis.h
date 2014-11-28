#ifndef PI0ANALYSIS_H
#define PI0ANALYSIS_H

#include <Riostream.h>

#include <TObject.h>
#include <TClonesArray.h>
#include <TPostScript.h>
#include <TH1F.h>
#include <TH2F.h>
#include <TGraphErrors.h>
#include <TFile.h>
#include <TTree.h>

class MyEvent;

class AnaCuts;
class EventMixer;

class Pi0Analysis : public TObject{
 protected:
  TFile *mFile;
  TFile *mFileOut;

  std::ofstream fout_mb;
  std::ofstream fout_ht1;
  std::ofstream fout_ht2;

  TTree *myEventTree;
  AnaCuts *cuts;
  EventMixer *mixer;
  TArrayI isHot;

  MyEvent *ev; //!
  TPostScript *ps;
  TPostScript *ps2;

  Bool_t isMC;
  Bool_t isPythia;
  Bool_t isDAU;
  Bool_t isPP05;
  Bool_t isAUAU200;
  Bool_t isHIJING;

  Bool_t noMINBIAS;

  Float_t WEIGHT;
  Int_t iev_0;
  Int_t iev_1;
  Int_t iev_2;


  Int_t runPrev;
  Int_t startdatePrev;
  Int_t starttimePrev;

  Int_t numberOfMB;
  Int_t numberOfHT1;
  Int_t numberOfHT2;
  Int_t nMBinRun;
  Int_t nHT1inRun;
  Int_t nHT2inRun;

  Float_t psMB;
  Float_t psHT1;
  Float_t psHT2;

  Float_t psHT1_eff;
  Float_t psHT1_eff2;
  Float_t nMB_eff;

  //event histos
  TH1F *h_vzMB;
  TH1F *h_vzHT1;
  TH1F *h_vzHT2;
  TH1F *h_ratioMB;
  TH1F *h_ratioHT1;
  TH1F *h_ratioHT2;
  TH1F *h_nrefmult;
  TH1F *h_trigidHT1;
  TH1F *h_trigidHT2;
  TH1F *h_trigidHT1aftercut;
  TH1F *h_trigidHT2aftercut;
  TH1F *h_events;

  TH2F *h_bbcVsTpc;
  TH2F *h_bbcVsTpcCorr;
  TH1F *h_bbcRes;

  TH2F *h_HT1adc_id;
  TH2F *h_HT2adc_id;

  //point
  TH2F *h_etaphi;
  TH2F *h_rapphi;
  TH1F *h_dist;
  TH2F *h_dist2DMB;
  TH2F *h_dist2DHT1;
  TH2F *h_dist2DHT2;
  TH2F *h_dist2DMBpions;
  TH2F *h_dist2DHT1pions;
  TH2F *h_dist2DHT2pions;

  TH2F *h_nstripsETA;
  TH2F *h_nstripsPHI;

  TH2F *h_clusterWidth;
  TH2F *h_energyRatio;

  TH2F *h_smdeta1;
  TH2F *h_smdphi1;
  TH2F *h_smdeta2;
  TH2F *h_smdphi2;

  //pi0
  TH2F *h_asymmMB;
  TH2F *h_asymmHT1;
  TH2F *h_asymmHT2;
  TH2F *h_asymmMBbg;
  TH2F *h_asymmHT1bg;
  TH2F *h_asymmHT2bg;

  TH2F *h_minvMB;
  TH2F *h_minvHT1;
  TH2F *h_minvHT2;
  TH2F *h_minvMB_mixed;
  TH1F *h_yieldMB;
  TH1F *h_yieldHT1;
  TH1F *h_yieldHT2;

  TH1F *h_pionsVsEtaMB;

  //gamma
  TH1F *h_gammaMB;
  TH1F *h_gammaHT1;
  TH1F *h_gammaHT2;

  //pythia
  TH1F *h_pythiaPartonPt;
  TH1F *h_pythiaPions;
  TH1F *h_pythiaPionsMB;
  TH1F *h_pythiaPionsHT1;
  TH1F *h_pythiaPionsHT2;

  TH1F *h_adcHT1;
  TH1F *h_adcHT2;

  TH2F *h_EvsE;


  TObjArray *c_array;


 public:

  TH1F *h_neutronsMB;
  TH1F *h_mcneutronsMB;
  TH1F *h_mcneutronsWeightMB;
  TH1F *h_neutronsHT1;
  TH1F *h_mcneutronsHT1;
  TH1F *h_mcneutronsWeightHT1;
  TH1F *h_neutronsHT2;
  TH1F *h_mcneutronsHT2;
  TH1F *h_mcneutronsWeightHT2;
  Pi0Analysis(const Char_t *, const Char_t *, const Char_t*);
  ~Pi0Analysis();
  Int_t init(const Char_t *);
  Int_t make(Int_t evmax=0, const Char_t* d="default");
  Int_t finish();
  void getYield();
  
  TH1F *getYield(TH2F *h,const Char_t *opt="none");

  Float_t getWeight(Float_t val=0.);

  inline void setMC(Bool_t val) {isMC=val;}
  inline void setPythia(Bool_t val) {isPythia=val;}
  inline void setHijing(Bool_t val) {isHIJING=val;}
  inline Int_t getNmb(){return numberOfMB;}
  inline Int_t getNht1(){return numberOfHT1;}
  inline Int_t getNht2(){return numberOfHT2;}

  inline void setNoMINBIAS(Bool_t val) {noMINBIAS=val;}

  void printPrescales();
  void storeCanvases(const Char_t *val="bla.root");

  ClassDef(Pi0Analysis,1)
};
#endif
