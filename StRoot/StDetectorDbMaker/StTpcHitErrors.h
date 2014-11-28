#ifndef StTpcHitErrors_h
#define StTpcHitErrors_h
#include "TMDFParameters.h"

class StTpcHitErrors : public TObject {
 public:
  static StTpcHitErrors* instance();
  //  virtual void  calculateError(Double_t _z,  Double_t _eta, Double_t _tanl, Double_t x,Double_t &ecross, Double_t &edip) const;
  StTpcHitErrors(Int_t nXZ = 2, Int_t Sec = 2, Int_t Row = 2, Int_t MS = 2, Int_t Prompt = 2) :
    fXZ(nXZ), fSec(Sec), fRow(Row), fMS(MS), fPrompt(Prompt), 
    fNxz(fXZ*fSec*fRow*fMS*fPrompt), fMDFXZ(fNxz) {
    fgInstance = this; fMDFXZ.SetOwner(kTRUE); 
  }
  virtual ~StTpcHitErrors() {fgInstance = 0;}
  Int_t Index(Int_t iXZ, Int_t sec, Int_t row, Int_t ms, Int_t prompt) {
    Int_t indx = prompt + fPrompt*(ms + fMS*(row + fRow*(sec + fSec*iXZ)));
    return indx;
  }
  TMDFParameters *Get(Int_t iXZ, Int_t sec, Int_t row, Int_t ms, Int_t prompt) {
    Int_t indx = Index(iXZ,sec,row,ms,prompt);
    TMDFParameters::SetCurrent((TMDFParameters *) fMDFXZ[indx]);
    return (TMDFParameters *) fMDFXZ[indx];
  }
  TMDFParameters *GetMu     (Int_t iXZ, Int_t sec, Int_t row, Int_t prompt) {return Get(iXZ,sec,row,0,prompt);}
  TMDFParameters *GetSigmaSQ(Int_t iXZ, Int_t sec, Int_t row, Int_t prompt) {return Get(iXZ,sec,row,1,prompt);}
  Double_t calcError(Int_t iXZ, Int_t sec, Int_t row, Double_t _z,  Double_t _eta, Double_t _tanl, Int_t Npads, Int_t Ntmbks, Double_t AdcL, Double_t xPad);
  void Set(Int_t iXZ, Int_t sec, Int_t row, Int_t ms, Int_t prompt, TMDFParameters *mdfX) {fMDFXZ.AddAtAndExpand(mdfX,Index(iXZ,sec,row,ms,prompt));}
 private:
  Int_t fXZ;     // in X or Z
  Int_t fSec;    // for West and East
  Int_t fRow;    // for Inner and Outer
  Int_t fMS;     // = 0 for mean, = 1 for log(sigma)
  Int_t fPrompt; // = 0 for drift, = 1 for prompt
  Int_t fNxz;
  TObjArray fMDFXZ; 
  static StTpcHitErrors* fgInstance;
  ClassDef(StTpcHitErrors,1) //
};
#endif
