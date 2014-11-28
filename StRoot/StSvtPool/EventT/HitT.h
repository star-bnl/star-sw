#ifndef __HIT__
#define __HIT__
//#define __USE_GLOBAL__
#include <string.h>
#include "TObject.h"
class StHit;
class HitT : public TObject {
 private:
  Char_t start;
  Int_t Id;
  Int_t sector, barrel, layer, ladder, wafer, hybrid, rdo; // SSD: barrel = layer = hybrid = 0
  Double32_t xG, yG, zG;    // hit Global from StEvent
  Double32_t xGC, yGC, zGC; // hit Global from local
  Double32_t xL, yL, zL;    // hit in Ladder CS
  Double32_t u, v, w;       // hit in Local (Wafer) xL == u_m, yL == v_m
  Double32_t tuP, tvP;      // tangs 
  Double32_t uP, vP;        // prediction in Wafer CS
  Double32_t pT, pMom;      // track 
  Double32_t xPG, yPG, zPG; // Prediction in Global CS
  Double32_t cxPG, cyPG, czPG; // Predicted direction cos in Global
  Double32_t wGu, wGv, wGw; // Global direction for detector plane
  Double32_t xPL, yPL, zPL; // Ladder
#ifdef __USE_GLOBAL__
  Double32_t uPGl, vPGl;        // prediction in Wafer CS
  Double32_t tuPGl, tvPGl;      // tangs 
  Double32_t xPGlG, yPGlG, zPGlG; // Prediction in Global CS
  Double32_t cxPGlG, cyPGlG, czPGlG; // Predicted direction cos in Global
  Double32_t xPGlL, yPGlL, zPGlL; // Ladder
#endif
  //  Double32_t cxPL, cyPL, czPL; // Ladder
#if 0
  Bool_t     fValidDerivatives;
  Double32_t duPdxV,duPdyV,duPdzV,duPddip,duPdphi,duPdRho; // derivatives uP wrt xV,yV,zV,1/pT,dip,phi
  Double32_t dvPdxV,dvPdyV,dvPdzV,dvPddip,dvPdphi,dvPdRho; // derivatives vP wrt xV,yV,zV,1/pT,dip,phi
#endif
  Double32_t uM, vM;
  Double32_t anode, timeb;
  Int_t      NoHitPerTrack;
  Double32_t uD, vD;
  Double32_t uHat;
  Double32_t vHat;
  Int_t      NofHits; // total no. of hits per wafer
  Int_t      NofFHits;// total no. of fitted hits per wafer
  Int_t      isFitted;
  Int_t      isTrack; 
  Int_t      isUsedInFit;
  UInt_t     hitFlag;
  Char_t end;
 public:
  HitT(Int_t B = 0, Int_t L = 0, Int_t l = 0, Int_t W = 0, Int_t H = 0,
      Double32_t X = 0, Double32_t Y = 0, Double32_t Z = 0,
      Double32_t XL = 0, Double32_t YL = 0, Double32_t ZL = 0) {
    memset(&start, 0, &end - &start);
    SetId(B,L,l,W,H); Set(X,Y,Z,XL,YL,ZL);
  }
  
  virtual ~HitT() {}
  void Set(Double32_t X, Double32_t Y, Double32_t Z,
      Double32_t XL = 0, Double32_t YL = 0, Double32_t ZL = 0) {
    xG = X; yG = Y; zG = Z; 
    uM = XL; vM = YL; w = ZL;
  }
  void SetHitFlag(const UInt_t flag) {hitFlag = flag;}
  void SetL(Double32_t X, Double32_t Y, Double32_t Z) {xL = X; yL = Y; zL = Z;}
  void SetGC(Double32_t X, Double32_t Y, Double32_t Z) {xGC = X; yGC = Y; zGC = Z;}
  void SetLM(Double32_t X, Double32_t Z) {u = X; v = Z;}
  void SetAnode(Double32_t p=0) {anode=p;}
  void SetTimeB(Double32_t p=0) {timeb=p;}
  void SetId(Int_t B = 0, Int_t L = 0, Int_t l = 0, Int_t W = 0, Int_t H = 0);
#if 0
  void SetId(StHit *HitT);
#endif
  void Set(Double32_t *xyzG, Double32_t *xyzL) {Set(xyzG[0],xyzG[1],xyzG[2],xyzL[0],xyzL[1],xyzL[2]);}
  void SetpT(Double32_t p) {pT = p;}
  void SetMom(Double32_t p) {pMom = p;}
  void SetUVPred(Double32_t u, Double32_t v) {uP = u; vP = v;}
  void SettUVPred(Double32_t tu, Double32_t tv) {tuP = tu; tvP = tv;}
  void SetWG(Double32_t wu, Double32_t wv, Double32_t ww) { wGu = wu; wGv = wv; wGw = ww;}
  //  void SetWL(Double32_t wx, Double32_t wy, Double32_t wz) { wLx = wx; wLy = wy; wLz = wz;}
#if 0
  void SetValidDerivatives(Bool_t p=kTRUE) {fValidDerivatives = p;}
  void SetDerivatives(Double32_t *der) {Double32_t *d = &duPdxV; for (Int_t i = 0; i < 12; i++) d[i] = der[i];}
#endif
  void SetXyzG(const Double_t *x) {Double32_t *xyzPG = &xPG; for (Int_t i = 0; i < 3; i++) xyzPG[i] = x[i];}
  void SetDirG(const Double_t *x) {Double32_t *dirPG = &cxPG;for (Int_t i = 0; i < 3; i++) dirPG[i] = x[i];}
  void SetXyzL(const Double_t *x) {Double32_t *xyzPL = &xPL; for (Int_t i = 0; i < 3; i++) xyzPL[i] = x[i];}
  void SetRDO(Int_t r) {rdo = r;}
  void SetuvD(Double_t u, Double_t v) {uD = u; vD = v;};
#ifdef __USE_GLOBAL__
  void SetUVPredGl(Double32_t u, Double32_t v) {uPGl = u; vPGl = v;}
  void SettUVPredGl(Double32_t tu, Double32_t tv) {tuPGl = tu; tvPGl = tv;}
  void SetXyzGl(const Double_t *x) {Double32_t *xyzPG = &xPGlG; for (Int_t i = 0; i < 3; i++) xyzPG[i] = x[i];}
  void SetDirGl(const Double_t *x) {Double32_t *dirPG = &cxPGlG;for (Int_t i = 0; i < 3; i++) dirPG[i] = x[i];}
  void SetXyzGlL(const Double_t *x) {Double32_t *xyzPL = &xPGlL; for (Int_t i = 0; i < 3; i++) xyzPL[i] = x[i];}
#endif
  void SetHitPerTrack(Int_t k) {NoHitPerTrack = k;}
  void SetuHat(Double_t u) {uHat = u;}
  void SetvHat(Double_t v) {vHat = v;}
  void SetNofHits(Int_t n) {NofHits = n;}
  void SetNofFHits(Int_t n) {NofFHits = n;}
  void SetisFitted(Int_t k=1) {isFitted = k;}
  void SetisTrack(Int_t k=1) {isTrack = k;}
  void SetUsedInFit(Int_t k=0) {isUsedInFit = k;}
  UInt_t      GetHitFlag()     const {return hitFlag;}
  Double32_t  GetU()           const {return u;}
  Double32_t  GetV()           const {return v;}
  Double32_t  GetuD()          const {return uD;}
  Double32_t  GetvD()          const {return vD;}
  Double32_t *GetXyzP()              {return &xPG;}
  Double32_t *GetXyzL()              {return &xPL;}
  Double32_t *GetXyzW()              {return &xPL;}
  Double32_t  GetPredtU()      const {return tuP;}
  Double32_t  GetPredtV()      const {return tvP;}
  Double32_t  GetPredU()       const {return uP;}
  Double32_t  GetPredV()       const {return vP;}
#ifdef __USE_GLOBAL__

  Double32_t *GetXyzPGl()              {return &xPGlG;}
  Double32_t *GetXyzLGl()              {return &xPGlL;}
  Double32_t *GetXyzWGl()              {return &xPGlL;}
  Double32_t  GetPredGltU()      const {return tuPGl;}
  Double32_t  GetPredGltV()      const {return tvPGl;}
  Double32_t  GetPredGlU()       const {return uPGl;}
  Double32_t  GetPredGlV()       const {return vPGl;}
#endif
  Int_t       Barrel()         const {return barrel;}
  Int_t       Layer()          const {return layer;}
  Int_t       Ladder()         const {return ladder;}
  Int_t       Wafer()          const {return wafer;}
  Int_t       GetId()          const {return Id;}
#if 0
  const Double32_t *GetDerivatives() const {return &duPdxV;}
  Bool_t      ValidDerivatives() const {return fValidDerivatives;}
#endif
  virtual void Print(Option_t *opt="") const;
  ClassDef(HitT,1)
};
#endif
