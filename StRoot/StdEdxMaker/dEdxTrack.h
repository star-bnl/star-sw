#ifndef dEdxTrack_h
#define dEdxTrack_h

#include "dEdxPoint.h"
#include "TClonesArray.h"
class Point : public TObject {
 private: 
 public:
  Point() {};
  Point(dEdx_t point); 
  virtual ~Point() {};
  Int_t    sector;
  Int_t    row;
  Int_t    pad;
  Int_t    Fee;
  Double_t dx;
  Double_t dE;
  Double_t dEdx; 
  Double_t dEdxL;  // log of dEdx
  Double_t dEdxN; // normolized to BB
  Double_t dETot; 
  Double_t dEU;
  Double_t dEdxU; 
  Double_t dEdxLU;  // log of dEdx
  Double_t dEdxNU; // normolized to BB
  Double_t xyz[3];
  Double_t Prob; 
  Double_t SigmaFee;
  Double_t xscale;
  Double_t dEIpad;  // total charge integrated so far in the pad
  Double_t dEI3pad; // total charge integrated so far in the pad +/-
  Double_t dEIrow;  // total charge integrated so far in the row
  Double_t dETrow;  // total charge not integrated (time bucket only) in the row
  Double_t dET3row; // total charge not integrated (+0 + 2 time buckets only) in the row
  Double_t dET5row; // total charge not integrated (+0 + 4 time buckets only) in the row
  Double_t zdev; 
  Double_t dY;      // Projection on the wire
  Double_t RMS;     // rms from volume charge

  ClassDef(Point,1)
};

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
