#ifndef St_dEdx_t_h
#define St_dEdx_t_h

#include "TObject.h"
//________________________________________
class dEdx_t : public TObject {
 public:
  dEdx_t() {}
  virtual ~dEdx_t() {}
  /* U(uncorrected) -> R(ADC nonlinearity) ->  Z(Drift distance) -> P(ressure) -> T(ime ?) -> 
     S(ecRow) ->  M(ultiplicity charge) */
  Int_t    sector;
  Int_t    row;
  Int_t    pad;
  Int_t    Fee;
  Double_t dE;
  Double_t dx;
  Double_t dEdx;   // after all corrections
  Double_t dEdxL;  // log of dEdx
  Double_t dEdxN;  // normolized to BB
  Double_t dEU;    // before correction (only Scale2keV scale)
  Double_t dER;    // after row correction
  Double_t dERdx;    // after row correction
  Double_t dERdxL;    // after row correction
  Double_t dEUdx; 
  Double_t dEUdxL; // log of dEdx
  Double_t dEUdxN;  // normolized to BB
  Double_t dEP;    // after Pressure correction
  Double_t dEPdx; 
  Double_t dEPdxL; 
  Double_t dET;    // after TimeScale
  Double_t dETdx; 
  Double_t dEdxLT; 
  Double_t dETdxN;  // normolized to BB
  Double_t dES;    // after TimeScale + SecRow corrections
  Double_t dESdx; 
  Double_t dEdxLS; 
  Double_t dESdxN;  // normolized to BB
  Double_t dEZ;    // after TimeScale + SecRow + Sec Z corrections
  Double_t dEZdx; 
  Double_t dEZdxL; 
  Double_t dEZdxN;  // normolized to BB
  Double_t dEM;     // after TimeScale + SecRow + Sec Z + Multipicity 
  Double_t dEMdx; 
  Double_t dEMdxL; 
  Double_t dEMdxN;  // normolized to BB
  Double_t dEdxNB;  // normolized to Bichsel
  Double_t dETot; 
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
  Double_t zP;      // the most probable value from Bichsel
  Double_t sigmaP;  // sigma from Bichsel
  //  ClassDef(dEdx_t,0); 
}; 
#endif 
