#ifndef ANACUTS_H
#define ANACUTS_H

#include <TArrayI.h>
#include <TArrayD.h>
#include <TObject.h>
#include <TVector3.h>

class MyEvent;
class MyPoint;

class AnaCuts : public TObject{

 public:
  //vertex cut
  Float_t vertexxCUT;
  Float_t vertexyCUT;
  Float_t vertexzCUT;
  //track assoc.
  Float_t neutralCUT;
  Float_t photonCUT;
  //cluster smd
  Int_t etaHitsCUT;
  Int_t phiHitsCUT;
  Float_t energyRatioCUT;// E_bsmd/E_btow
  //det. eta
  Float_t etaMinCUT;
  Float_t etaMaxCUT;
  //rap.
  Float_t rapidityMinCUT;
  Float_t rapidityMaxCUT;
  //rap pion.
  Float_t rapPionMinCUT;
  Float_t rapPionMaxCUT;
  //software trigger
  Float_t softTrigHT1;
  Float_t softTrigHT2;

  Float_t ratioCUT;

  //hot towers
  TArrayI isHot;

  Float_t asymmetryCUT;
  Int_t ht1AdcCUT;
  Int_t ht2AdcCUT;

  //pt bins,minv bins
  Int_t nPtBinsMB;
  TArrayD ptBinsMB;
  Int_t nPtBinsHT1;
  TArrayD ptBinsHT1;
  Int_t nPtBinsHT2;
  TArrayD ptBinsHT2;
  Int_t nPtBinsEffMB;
  TArrayD ptBinsEffMB;
  Int_t nPtBinsEffHT1;
  TArrayD ptBinsEffHT1;
  Int_t nPtBinsEffHT2;
  TArrayD ptBinsEffHT2;
  Int_t nMinvBinsMB;
  Float_t mInvLowMB;
  Float_t mInvHighMB;
  Int_t nMinvBinsHT1;
  Float_t mInvLowHT1;
  Float_t mInvHighHT1;
  Int_t nMinvBinsHT2;
  Float_t mInvLowHT2;
  Float_t mInvHighHT2;

  //efficiency
  Int_t nPtBinsEff;
  TArrayD ptBinsEff;

  //peak integration
  Float_t timesSigma;

  AnaCuts(const char* flag="dAu");
  ~AnaCuts();
  void printCuts();
  Bool_t isPointOK(MyPoint*,TVector3);
  Bool_t isEventOK(MyEvent*,const char*);
  ClassDef(AnaCuts,1)
};

#endif
