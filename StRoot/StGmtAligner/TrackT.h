#ifndef __TRACK__
#define __TRACK__
#define NSP 1000
#include <string.h>
#include "TObject.h"
#include "TMath.h"
#include "HitT.h"
class TrackT : public TObject {
  
 private:
  Char_t          beg;
  Double32_t      fInvpT;        //signed
  Double32_t      fTanL;
  Double32_t      fPhi;
  Double32_t      fRho;
#ifdef __USE_GLOBAL__
  Double32_t      fInvpTGl;        //signed
  Double32_t      fTanLGl;
  Double32_t      fPhiGl;
  Double32_t      fRhoGl;
#endif
  UInt_t          fNpoint;       //Number of fitted points for this track
  UInt_t          fNPpoint;      //Number of possible points for this track
  Short_t         fValid;        //Validity criterion
  UInt_t          fNsp;          //Number of points for this track with a special value
  UInt_t          fIdHitT[NSP];   //Index of HitT in fHitT array
  Double32_t      fdEdx;  
  Double32_t      fLength;
  Char_t          end;
 public:
  TrackT() { Clear(); }
  virtual ~TrackT() {Clear();}
  void          Clear(Option_t *option="") {if (option); memset(&beg, 0, &end - &beg);}
  Double32_t    GetpX()      { return GetpT()*TMath::Cos(fPhi);}
  Double32_t    GetpY()      { return GetpT()*TMath::Sin(fPhi);}
  Double32_t    GetpZ()      { return GetpT()*fTanL;}
  Double32_t    GetInvpT()   { return fInvpT;}
  Double32_t    GetTanL()    { return fTanL;}
  Double32_t    GetDip()     { return TMath::ATan(fTanL);}
  Double32_t    GetPhi()     { return fPhi;}
  Double32_t    GetRho()     { return fRho;}
  Double32_t    GetpT()      { return TMath::Abs(fInvpT) > 1.e-7 ? 1./TMath::Abs(fInvpT): 1e7; }
  Double32_t    GetMomentum(){ return GetpT()*TMath::Sqrt(1. + fTanL*fTanL);}
  UInt_t        GetNpoint()  { return fNpoint; }
  UInt_t        GetNPpoint()  { return fNPpoint; }
  Short_t       GetCharge()  { return (Short_t) TMath::Sign(1., fInvpT); }
  Short_t       GetValid()   { return fValid; }
  UInt_t        GetN()       { return fNsp; }
  const UInt_t *GetIndx()   const { return fIdHitT;}
  Int_t         GetHitTId(UInt_t i=0) {return i < fNsp ? ((Int_t) fIdHitT[i])-1 : -1;}
  Double32_t    GetdEdx()    {return fdEdx;}
  Double32_t    GetLegth()   {return fLength;}
  virtual void SetInvpT(Double32_t p)  {fInvpT = p; }
  virtual void SetDip(Double32_t p)  {fTanL = TMath::Tan(p); }
  virtual void SetTanL(Double32_t p)  {fTanL = p; }
  virtual void SetPhi(Double32_t p)  {fPhi = p; }
  virtual void SetRho(Double32_t p)  {fRho = p; }
#ifdef __USE_GLOBAL__

  virtual void SetInvpTGl(Double32_t p)  {fInvpTGl = p; }
  virtual void SetDipGl(Double32_t p)  {fTanLGl = TMath::Tan(p); }
  virtual void SetTanLGl(Double32_t p)  {fTanLGl = p; }
  virtual void SetPhiGl(Double32_t p)  {fPhiGl = p; }
  virtual void SetRhoGl(Double32_t p)  {fRhoGl = p; }
#endif
  virtual void SetNpoint(UInt_t p)     {fNpoint = p; }
  virtual void SetNPpoint(UInt_t p)    {fNPpoint = p; }
  virtual void SetValid(Short_t p=1)   {fValid = p; }
  virtual void SetN(UInt_t n) {if (n <= NSP) fNsp = n; else fNsp = NSP;}
  virtual void SetHitTId(UInt_t i) {fIdHitT[fNsp] = i+1; if ( fNsp < NSP) fNsp++;}
  virtual void SetdEdx(Double_t I70, Double_t L) {fdEdx = I70; fLength = L;}
  virtual void Print(Option_t *opt="") const;
  ClassDef(TrackT,2)
};
#endif
