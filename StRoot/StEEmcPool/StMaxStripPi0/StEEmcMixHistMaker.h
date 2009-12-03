#ifndef __StEEmcMixHistMaker_h__
#define __StEEmcMixHistMaker_h__

#include "StEEmcMixMaker.h"
#include "TString.h"

class TH1F;
class TH2F;

class StEEmcMixHistMaker : public StEEmcMixMaker { 

 public:
  StEEmcMixHistMaker(const Char_t *name);
  ~StEEmcMixHistMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();
  Int_t Finish();

  void setRange(Float_t min=0.1, Float_t max=0.18){ mMin=min; mMax=max; }

 private:
 protected:

  TH1F *hMass[3];
  TH1F *hEnergy[3];
  TH1F *hPT[3];
  TH1F *hZgg[3];
  TH1F *hPhigg[3];
  TH1F *hU1[3];
  TH1F *hU2[3];
  TH1F *hV1[3];
  TH1F *hV2[3];
  void book( TH1F **h, const Char_t *n, const Char_t *t, Int_t nbin, Float_t min, Float_t max );
  void fill( Int_t mode, StEEmcPair &pair );
  void subtract( TH1F **h, Float_t norm );

  Bool_t accept( StEEmcPair &pair ); 

  Float_t mMin;
  Float_t mMax;


  ClassDef(StEEmcMixHistMaker,1);

};

#endif
