#ifndef dEdxTrackY2_h
#define dEdxTrackY2_h
#include "StTpcdEdxCorrection.h" 
#include "TClonesArray.h"
class dEdxTrackY2 : public TObject {
 private:
  Int_t          fNPoint;
  TClonesArray  *fPoints;           //->
  
  static TClonesArray *fgPoints;
 public:
  dEdxTrackY2();
  virtual ~dEdxTrackY2();
  void AddPoint(dEdxY2_t &point);
  void Clear(Option_t *option = "");
  void Reset(Option_t *option = "");

  Int_t    sCharge;
  Double_t p;
  Double_t pX;
  Double_t pY;
  Double_t pZ;
  Double_t Z0;
  Double_t R0;
  Double_t Phi0;
  Int_t    NoFitPoints;
  Int_t    NdEdx;
  Int_t    N70;
  Double_t I70;
  Double_t D70;  
  Double_t TrackLength70;
  Double_t fitZ;
  Double_t fitdZ;
  Double_t TrackLength;

  Double_t PredP;
  Double_t Pred70P;
  Double_t devzFP;
  Double_t devz70P;
  Double_t chi2ToFP;
  Double_t pidProbP;

  Double_t PredK;
  Double_t Pred70K;
  Double_t devzFK;
  Double_t devz70K;
  Double_t chi2ToFK;
  Double_t pidProbK;
  Double_t Predpi;
  Double_t Pred70pi;
  Double_t devzFpi;
  Double_t devz70pi;
  Double_t chi2ToFpi;
  Double_t pidProbpi;
  Double_t PredE;
  Double_t Pred70E;
  Double_t devzFE;
  Double_t devz70E;
  Double_t chi2ToFE;
  Double_t pidProbE;
  ClassDef(dEdxTrackY2,1)
};
#endif
