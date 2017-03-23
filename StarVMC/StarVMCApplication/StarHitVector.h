#ifndef StarHitVector_h
#define StarHitVector_h
#include "TLorentzVector.h"
#include "THashList.h"
#include "TGeoMatrix.h"
class StarVMCDetector;

struct G3Vector_t {
  TLorentzVector xyzT; 
  TLorentzVector pxyzE;
  G3Vector_t & operator += (const G3Vector_t &p) {xyzT += p.xyzT; pxyzE += p.pxyzE; return *this;}
  G3Vector_t & operator *= (Double_t a) {xyzT *= a; pxyzE *= a; return *this;}
  G3Vector_t & operator /= (Double_t a) {xyzT *= 1./a; pxyzE *= 1./a; return *this;}
};
struct G3GLPair_t {
  G3Vector_t Global; // Mother   
  G3Vector_t Local;  // Daughter
  void Global2Local(TGeoHMatrix  *matrixC) {
    Double_t xm[3] = {Global.xyzT.X(), Global.xyzT.Y(), Global.xyzT.Z()};
    Double_t xd[3];
    matrixC->MasterToLocal(xm,xd);
    Local.xyzT = TLorentzVector(xd[0],xd[1],xd[2],Global.xyzT.T());
    Double_t pxm[3] = {Global.pxyzE.Px(), Global.pxyzE.Py(), Global.pxyzE.Pz()};
    Double_t pxd[3];
    matrixC->MasterToLocalVect(pxm,pxd);
    Local.pxyzE = TLorentzVector(pxd[0],pxd[1],pxd[2],Global.pxyzE.E());
  }
  G3GLPair_t & operator += (const G3GLPair_t &p) {Global += p.Global; Local += p.Local; return *this;}
  G3GLPair_t & operator *= (Double_t a) {Global *= a; Local *= a; return *this;}
  G3GLPair_t & operator /= (Double_t a) {Global *= 1./a; Local *= 1./a; return *this;}
};
struct GHit_t {
  StarVMCDetector *Detector;
  Int_t      NVL;
  Int_t      NUMBV[20];
  Int_t      IdType;
  Int_t      VolumeId;
  Int_t      Serial;
  Int_t      Goption;
  Int_t      Nva;
  Int_t      Nvb;
  Int_t      Charge;
  Double_t   Sleng;
  Int_t      iPart;
  Int_t      iTrack;
  Float_t    Mass;
  Float_t    AStep;
  Float_t    AdEstep;
  Float_t    birk;
  G3GLPair_t Current;// IsTrackEntering inwvol==1
  G3GLPair_t Entry;  // IsTrackEntering inwvol==1
  G3GLPair_t Exit;   // IsTrackExiting  inwvol==2 IsTrackStop istop==2
  G3GLPair_t Middle; // the middle position in local coordinate
};
#ifndef __CINT__
struct genhit_t {
  Int_t    id,trac,next,volume;
  union  {
    Float_t xhloc[3];
    Float_t x[3];
  };
  union  {
    Float_t xhglb[3];
    Float_t xx[3];
  };
  union  {
    Float_t chloc[3];
    Float_t c[3];
  };
  union  {
    Float_t pmomg;
    Float_t p;
  };
  union  {
    Float_t radi;
    Float_t r;
  };
  union  {
    Float_t rrad;
    Float_t rr;
  };
  Float_t phi;
  union {
    Float_t theta;
    Float_t the;
  };
  Float_t eta;
  Float_t tdr;
  Float_t tof;
  Float_t Slen;
  Float_t Step;
  Float_t Etot;
  Float_t Lgam;
  Float_t Lpto;
  Float_t Elos;
  Float_t User;
  Float_t Unkn[3];
};
#endif

#endif
