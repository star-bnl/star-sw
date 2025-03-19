#ifndef __StKFEvent__
#define __StKFEvent__
#include "Riostream.h"
#include "TObject.h"
#include "TClonesArray.h"
#include "StMuDstVtxT.h"
#include "StKFVertex.h"
#include "StVertexT.h"
#include "StVertexP.h"

class StKFEvent : public TObject {
public:
  StKFEvent() {
    if (!fgMuDstVtx) fgMuDstVtx = new TClonesArray("StVertexT", 100);
    fMuDstVtx = fgMuDstVtx; fNMuDstVtx = 0;
    if (!fgKFVtx) fgKFVtx = new TClonesArray("StVertexT", 100);
    fKFVtx = fgKFVtx; fNKFVtx = 0;
    if (!fgDKFPair) fgDKFPair = new TClonesArray("StVertexP", 100);
    fDKFPair = fgDKFPair; fNDKFPair = 0;
    if (!fgKFKFPair) fgKFKFPair = new TClonesArray("StVertexP", 100);
    fKFKFPair = fgKFKFPair; fNKFKFPair = 0;
  }
  virtual ~StKFEvent() {Clear();}
  void SetTemperature(Double_t T) {fTemperature = T;}
  void AddMuVtx(StMuDstVtxT &muDstVtx)    {TClonesArray &MuDstVtxs = *fMuDstVtx; new(MuDstVtxs[fNMuDstVtx++]) StVertexT(muDstVtx);}
  void AddKFVtx(StKFVertex  &kfVtx)       {TClonesArray &KFVtxs    = *fKFVtx;    new(KFVtxs[fNKFVtx++])       StVertexT(kfVtx);   }
  void AddDKFPair(Int_t i, Int_t j, StVertexT &muDstVtx, StVertexT  &kfVtx,  Double_t chi2 = 0) {
    TClonesArray &DKFPairs    = *fDKFPair;    new(DKFPairs[fNDKFPair++])   StVertexP(i,j,muDstVtx,kfVtx,chi2);   
  }
  void AddKFKFPair(Int_t i, Int_t j, StVertexT  &kfVtxI, StVertexT  &kfVtxJ, Double_t chi2 = 0) {
    TClonesArray &KFKFPairs    = *fKFKFPair;    new(KFKFPairs[fNKFKFPair++])       StVertexP(i,j,kfVtxI,kfVtxJ,chi2);   
  }
  void Clear(Option_t *option = "")       {
    fTemperature = 0;
    fNMuDstVtx = 0; fMuDstVtx->Clear(option); 
    fNKFVtx = 0; fKFVtx->Clear(option);
    fNDKFPair = 0; fDKFPair->Clear(option);
    fNKFKFPair = 0; fKFKFPair->Clear(option);
  }
  void Reset(Option_t */* option = "" */) {SafeDelete(fgMuDstVtx); SafeDelete(fgKFVtx); SafeDelete(fgDKFPair); SafeDelete(fgKFKFPair);}
  Int_t NoMuDstVtx()       {return fNMuDstVtx;}
  TClonesArray *MuDstVtx() {return fMuDstVtx;}
  Int_t NoKFVtx()          {return fNKFVtx;}
  TClonesArray *KFVtx()    {return fKFVtx;}
  Int_t NoDKFPair()        {return fNDKFPair;}
  TClonesArray *DKFPair()  {return fDKFPair;}
  Int_t NoKFKFPair()       {return fNKFKFPair;}
  TClonesArray *KFKFPair() {return fKFKFPair;}
private:
  Double_t fTemperature;
  Int_t fNMuDstVtx;
  Int_t fNKFVtx;
  Int_t fNDKFPair;
  Int_t fNKFKFPair;
  TClonesArray *fMuDstVtx;    //->
  TClonesArray *fKFVtx;       //->
  TClonesArray *fDKFPair;     //->
  TClonesArray *fKFKFPair;    //->
  static TClonesArray *fgMuDstVtx;
  static TClonesArray *fgKFVtx;
  static TClonesArray *fgDKFPair;
  static TClonesArray *fgKFKFPair;

  ClassDef(StKFEvent,1)
};
#endif
