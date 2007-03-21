#include <assert.h>
#include "TMath.h"
#include "TString.h"
#include "Stiostream.h"
#include "St_svtHybridDriftVelocityC.h"
ClassImp(St_svtHybridDriftVelocityC);
St_svtHybridDriftVelocityC *St_svtHybridDriftVelocityC::fgsvtHybridDriftVelocityC = 0;
Double_t St_svtHybridDriftVelocityC::mAnodePitch  = 0.0250;
Double_t St_svtHybridDriftVelocityC::mWaferLength = 2.9928;
Double_t St_svtHybridDriftVelocityC::mWaferWidth  = 3.0000;
Double_t St_svtHybridDriftVelocityC::mSamplingFrequency = 25000000.0;
static const Int_t NB    =  3;
static const Int_t NL    = 16;
static const Int_t NW    =  7;
static const Int_t NH    =  2;
static svtHybridDriftVelocity_st *pointers[3][16][7][2];
static svtHybridDriftVelocity_st *DataOld = 0;
//________________________________________________________________________________
St_svtHybridDriftVelocityC::St_svtHybridDriftVelocityC (St_svtHybridDriftVelocity *table) : TChair(table) {
  if (fgsvtHybridDriftVelocityC) delete fgsvtHybridDriftVelocityC; fgsvtHybridDriftVelocityC = this;
  Init();
}
//________________________________________________________________________________
void St_svtHybridDriftVelocityC::Init() {
  memset (&pointers[0][0][0][0], 0, NB*NL*NW*NH*sizeof(svtHybridDriftVelocity_st *));
  St_svtHybridDriftVelocity *Table =  (St_svtHybridDriftVelocity *) GetThisTable();
  if (Table) {
    svtHybridDriftVelocity_st *Data =  Table->GetTable();
    DataOld = Data;
    Int_t N = Table->GetNRows();
    for (Int_t j = 0; j < N; j++, Data++) {
      if (Data->barrel < 1 || Data->barrel > NB ||
	  Data->ladder < 1 || Data->ladder > NL ||
	  Data->wafer  < 1 || Data->wafer  > NW ||
	  Data->hybrid < 1 || Data->hybrid > NH) continue;
      pointers[Data->barrel-1][Data->ladder-1][Data->wafer-1][Data->hybrid-1] = Data;
    }
  }
}
//________________________________________________________________________________
svtHybridDriftVelocity_st *St_svtHybridDriftVelocityC::p(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid) {
  svtHybridDriftVelocity_st *p = 0;
  if (!  GetThisTable()) return p;
  if (((St_svtHybridDriftVelocity *)GetThisTable())->GetTable() != DataOld) Init();
  if (barrel >= 1 && barrel <=  NB &&
      ladder >= 1 && ladder <=  NL &&
      wafer  >= 1 && wafer  <=  NW && 
      hybrid >= 1 && hybrid <=  NH)  p = pointers[barrel-1][ladder-1][wafer-1][hybrid-1];
  return p;
}
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::CalcDriftLength(svtHybridDriftVelocity_st *p, Double_t timeBin) {
  if (! p) return -99;
  Double_t t = TMath::Min(p->tmax,TMath::Max(p->tmin,timeBin));
  return mWaferLength*(t - p->tmin)/(p->tmax - p->tmin);
}
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::UnCalcDriftLength(svtHybridDriftVelocity_st *p, Double_t x) {
  if (! p) return -99;
  Double_t d = TMath::Min(mWaferLength,TMath::Max(0.,x));
  return  p->tmin + (p->tmax - p->tmin)*d/mWaferLength;
}
//________________________________________________________________________________
Double_t St_svtHybridDriftVelocityC::DriftVelocity(svtHybridDriftVelocity_st *p) {
  return p ? mSamplingFrequency*mWaferLength/(p->tmax - p->tmin): -1;
}
