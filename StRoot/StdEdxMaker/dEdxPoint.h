#ifndef ST_dEdxPoint_Table
#define ST_dEdxPoint_Table

#include "TTable.h"
#include "Ttypes.h"
typedef struct
{   
  UInt_t sortId;
  Int_t id_track; /* multiple track instances                */
  Int_t FitFlag;
  Int_t iflag;
  Int_t sector;
  Int_t row;
  Int_t pad;
  Int_t timeBucket;
  Double_t xyz[3];
  Double_t dE;
  Double_t dEU; // before correction
  Double_t dx;
  Double_t dEIpad;  // total charge integrated so far in the pad
  Double_t dEI3pad; // total charge integrated so far in the pad +/-
  Double_t dEIrow;  // total charge integrated so far in the row
  Double_t dETrow;  // total charge not integrated (only time bucket) in the row
  Double_t dET3row; // total charge not integrated (+/- 1 time buckets only) in the row
  
  Double_t dET5row; // total charge not integrated (+/- 1 time buckets only) in the row
} dEdxPoint; 
//________________________________________
class St_dEdxPoint : public TTable
{
public:
    ClassDefTable(St_dEdxPoint, dEdxPoint)
    ClassDef(St_dEdxPoint,1) //C++ wrapper for <ucell> StAF table
};
//________________________________________
typedef struct {
  Int_t    sector;
  Int_t    row;
  Int_t    pad;
  Int_t    Fee;
  Double_t dE;
  Double_t dEU; // before correction
  Double_t dx;
  Double_t dEdx; 
  Double_t dEdxU; 
  Double_t dEdxP; // after pulser correction only
  Double_t dEdxL; // log of dEdx
  Double_t dEdxLU; // log of dEdx
  Double_t dEdxN; // normolized to BB
  Double_t dEdxNP; // normolized to BB
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

} dEdx_t;
#endif 
