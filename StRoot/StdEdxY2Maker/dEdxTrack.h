#ifndef dEdxTrack_h
#define dEdxTrack_h
#include "StTpcDb/StTpcdEdxCorrection.h" 
#include "TClonesArray.h"
class dEdxTrack : public TObject {
 private:
  Int_t          fNPoint;
  TClonesArray  *fPoints;           //->
  
  static TClonesArray *fgPoints;
 public:
  dEdxTrack();
  virtual ~dEdxTrack();
  void AddPoint(dEdx_t &point);
  void Clear(Option_t *option = "");
  void Reset(Option_t *option = "");

  Int_t    sCharge;
  Double_t p;
  Double_t Eta;
  Double_t R0;
  Double_t Z0;
  Double_t Phi0;
  Int_t    NoFitPoints;
  Int_t    N70;
  Double_t I70;
  Double_t TrackLength70;
  Int_t    N60;
  Double_t I60;
  Double_t TrackLength60;
  Int_t    NdEdx;
  Double_t chisq;
  Double_t fitZ;
  Double_t fitdZ;
  Double_t TrackLength;
  Double_t PredP;
  Double_t PredK;
  Double_t PredPi;
  Double_t PredE;
  ClassDef(dEdxTrack,1)
};
#endif
