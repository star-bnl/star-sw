#ifndef MYDECAY_H
#define MYDECAY_H

#include <TROOT.h>
#include <TSystem.h>
#include <TObject.h>
#include <TObjArray.h>
#include <TParticle.h>
#include <TH1F.h>
#include <TFile.h>
#include <TString.h>

class MyDecay : public TObject{
 protected:
  Bool_t isPP05;
  Bool_t isDAU;
  Double_t fETAMTSCALE;
  TString mFileName;
 public:
  MyDecay(const char* file="junk.root");
  ~MyDecay();
  TObjArray *decay(TParticle *part); 
  void doDecay(Int_t i);
  Double_t ptweight(Double_t x);
  Double_t ptweightETA(Double_t x);
  Double_t ptweightOMEGA(Double_t x);
  Double_t etaweight(Double_t x);
  Double_t phiweight(Double_t x);
  
  void setPP05(Bool_t val) {isPP05=val;}
  void setDAU(Bool_t val) {isDAU=val;}
  void setETAMTSCALE(Double_t val) {fETAMTSCALE=val;}

  ClassDef(MyDecay,1)
};

#endif
